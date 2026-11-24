import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const PORT = Number(process.env.PORT || 3000);
const HOST = '0.0.0.0';

function escapeHtml(str) {
  return String(str)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

function classifyTier(filename) {
  if (filename.includes('shinytest2')) return { id: 'e2e', label: 'Tier 3 · shinytest2 E2E' };
  if (filename.includes('module-') || filename.includes('app-shell')) return { id: 'server', label: 'Tier 2 · shiny::testServer' };
  return { id: 'unit', label: 'Tier 1 · Unit & SQLite Integration' };
}

function parseTestFiles() {
  const testDir = path.join(__dirname, 'tests', 'testthat');
  if (!fs.existsSync(testDir)) return [];

  const files = fs
    .readdirSync(testDir)
    .filter((f) => f.startsWith('test-') && f.endsWith('.R'))
    .sort();

  return files.map((filename) => {
    const fullPath = path.join(testDir, filename);
    const content = fs.readFileSync(fullPath, 'utf8');
    const lines = content.split(/\r?\n/);

    let covers = '';
    for (const line of lines.slice(0, 10)) {
      const m = line.match(/^#\s*Covers:\s*(.+)$/);
      if (m) {
        covers = m[1].trim();
        break;
      }
    }

    const cases = [];
    for (let i = 0; i < lines.length; i++) {
      const m = lines[i].match(/^test_that\(\s*"([^"]+)"\s*,/);
      if (m) {
        const descLines = [];
        let j = i + 1;
        while (j < lines.length && lines[j].trim().startsWith('#')) {
          descLines.push(lines[j].trim().replace(/^#\s?/, ''));
          j++;
        }
        cases.push({
          title: m[1],
          line: i + 1,
          description: descLines.join(' ')
        });
      }
    }

    return {
      filename,
      relPath: `tests/testthat/${filename}`,
      tier: classifyTier(filename),
      covers,
      lineCount: lines.length,
      cases
    };
  });
}

function parseRSourceSummary() {
  const rDir = path.join(__dirname, 'R');
  if (!fs.existsSync(rDir)) return [];

  return fs
    .readdirSync(rDir)
    .filter((f) => f.endsWith('.R'))
    .sort()
    .map((filename) => {
      const content = fs.readFileSync(path.join(rDir, filename), 'utf8');
      const lines = content.split(/\r?\n/);
      let nocovBlocks = 0;
      let inNocov = false;
      let nocovLines = 0;

      for (const line of lines) {
        if (/#\s*nocov\s+start/.test(line)) {
          inNocov = true;
          nocovBlocks++;
          continue;
        }
        if (/#\s*nocov\s+end/.test(line)) {
          inNocov = false;
          continue;
        }
        if (inNocov || /#\s*nocov\b/.test(line)) {
          nocovLines++;
        }
      }

      return {
        filename: `R/${filename}`,
        lineCount: lines.length,
        nocovBlocks,
        nocovLines
      };
    });
}

const FINAL_ASSETS = {
  logo: {
    file: path.join(__dirname, 'man', 'figures', 'logo.png'),
    mime: 'image/png'
  },
  appLogo: {
    file: path.join(__dirname, 'inst', 'www', 'logo.png'),
    mime: 'image/png'
  },
  favicon: {
    file: path.join(__dirname, 'inst', 'www', 'favicon.png'),
    mime: 'image/png'
  },
  reference: {
    file: path.join(__dirname, 'public', 'assets', 'aistudio', 'favicon-96x96_423ef6e7.png'),
    mime: 'image/png'
  }
};

function buildHtml() {
  const suites = parseTestFiles();
  const sources = parseRSourceSummary();
  const totalCases = suites.reduce((acc, s) => acc + s.cases.length, 0);
  const totalTestLines = suites.reduce((acc, s) => acc + s.lineCount, 0);
  const totalNocovLines = sources.reduce((acc, s) => acc + s.nocovLines, 0);

  const suiteCardsHtml = suites
    .map(
      (s) => `
      <section class="suite-card" data-tier="${escapeHtml(s.tier.id)}">
        <header class="suite-header">
          <div>
            <span class="tier-tag tier-${escapeHtml(s.tier.id)}">${escapeHtml(s.tier.label)}</span>
            <h3>${escapeHtml(s.relPath)}</h3>
            ${s.covers ? `<p class="covers">Covers: <code>${escapeHtml(s.covers)}</code></p>` : ''}
          </div>
          <div class="suite-meta">
            <strong>${s.cases.length}</strong> test_that() blocks · ${s.lineCount} lines
          </div>
        </header>
        <ol class="case-list">
          ${s.cases
            .map(
              (c) => `
            <li class="case-item">
              <div class="case-title-row">
                <span class="case-title">${escapeHtml(c.title)}</span>
                <span class="case-line">L${c.line}</span>
              </div>
              ${c.description ? `<p class="case-desc">${escapeHtml(c.description)}</p>` : ''}
            </li>`
            )
            .join('')}
        </ol>
      </section>`
    )
    .join('');

  const sourceRowsHtml = sources
    .map(
      (src) => `
      <tr>
        <td><code>${escapeHtml(src.filename)}</code></td>
        <td class="num">${src.lineCount}</td>
        <td class="num">${src.nocovLines > 0 ? `<span class="nocov-note">${src.nocovLines} lines (MySQL / unreachable)</span>` : '0'}</td>
      </tr>`
    )
    .join('');

  return `<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>CodingManageR · Indigo Branding &amp; Test Suite Explorer</title>
  <link rel="icon" type="image/png" href="/assets/favicon" />
  <style>
    :root {
      --bg: #F6F7FB;
      --surface: #FFFFFF;
      --subtle: #EEF0F6;
      --ink: #1F2937;
      --heading: #1C234A;
      --muted: #525B70;
      --border: #D9DDEA;
      --border-strong: #C2C8DC;
      --indigo: #33418F;
      --indigo-hover: #26316E;
      --indigo-soft: #EEF0FA;
      --accent: #166534;
      --accent-soft: #ECFDF3;
      --blue: #33418F;
      --amber: #92400E;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      background: var(--bg);
      color: var(--ink);
      line-height: 1.5;
    }
    .topbar {
      background: var(--surface);
      border-top: 3px solid var(--indigo);
      border-bottom: 1px solid var(--border);
      position: sticky;
      top: 0;
      z-index: 20;
    }
    .topbar-inner {
      max-width: 1120px;
      margin: 0 auto;
      padding: 12px 24px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
      flex-wrap: wrap;
    }
    .brand-wrap {
      display: inline-flex;
      align-items: center;
      gap: 10px;
    }
    .brand-wrap img {
      height: 32px;
      width: auto;
      display: block;
    }
    .brand {
      font-weight: 700;
      font-size: 1.05rem;
      color: var(--indigo);
      letter-spacing: -0.015em;
    }
    .nav-tabs { display: flex; gap: 8px; }
    .nav-tab {
      border: 1px solid var(--border);
      background: var(--surface);
      color: var(--muted);
      padding: 7px 14px;
      border-radius: 6px;
      font-size: 0.88rem;
      font-weight: 600;
      cursor: pointer;
    }
    .nav-tab.active {
      background: var(--indigo);
      border-color: var(--indigo);
      color: #FFFFFF;
    }
    .wrap { max-width: 1120px; margin: 0 auto; padding: 28px 24px 64px; }
    .panel { display: none; }
    .panel.active { display: block; }
    .hero {
      display: flex;
      justify-content: space-between;
      align-items: flex-end;
      gap: 24px;
      flex-wrap: wrap;
      border-bottom: 1px solid var(--border);
      padding-bottom: 20px;
      margin-bottom: 24px;
    }
    h1 { margin: 0 0 6px; font-size: 1.65rem; color: var(--heading); letter-spacing: -0.02em; }
    h2 { margin: 28px 0 12px; font-size: 1.15rem; color: var(--heading); }
    .subtitle { margin: 0; color: var(--muted); max-width: 68ch; font-size: 0.94rem; }
    .ref-box {
      display: flex;
      align-items: center;
      gap: 12px;
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 10px 14px;
    }
    .ref-box img { width: 48px; height: 48px; object-fit: contain; display: block; }
    .ref-label { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.05em; color: var(--muted); font-weight: 700; }
    .ref-title { font-size: 0.9rem; font-weight: 600; color: var(--heading); }
    .showcase-grid {
      display: grid;
      grid-template-columns: 1.1fr 1fr;
      gap: 20px;
      margin-bottom: 24px;
    }
    @media (max-width: 820px) { .showcase-grid { grid-template-columns: 1fr; } }
    .card {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 10px;
      overflow: hidden;
    }
    .card-head {
      padding: 14px 18px;
      border-bottom: 1px solid var(--border);
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 12px;
    }
    .card-head h3 { margin: 0; font-size: 0.98rem; color: var(--heading); }
    .bg-switcher { display: flex; gap: 6px; }
    .bg-btn {
      border: 1px solid var(--border);
      background: var(--surface);
      color: var(--ink);
      padding: 4px 10px;
      border-radius: 5px;
      font-size: 0.78rem;
      font-weight: 600;
      cursor: pointer;
    }
    .bg-btn.active { background: var(--indigo); border-color: var(--indigo); color: #FFFFFF; }
    .logo-stage {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 32px 24px;
      transition: background-color 0.15s ease;
    }
    .stage-light { background: #FFFFFF; }
    .stage-subtle { background: #EEF0F6; }
    .stage-dark { background: #18202C; }
    .logo-stage img { width: 240px; height: auto; display: block; }
    .card-body { padding: 16px 18px; border-top: 1px solid var(--border); }
    .scale-strip { display: flex; align-items: flex-end; gap: 20px; flex-wrap: wrap; }
    .scale-item { display: flex; flex-direction: column; align-items: center; gap: 5px; font-size: 0.74rem; color: var(--muted); }
    .scale-item img { display: block; object-fit: contain; }
    .swatches { display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; }
    .swatch {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 8px 10px;
      border: 1px solid var(--border);
      border-radius: 6px;
      background: #FAFBFD;
    }
    .swatch-box { width: 28px; height: 28px; border-radius: 5px; border: 1px solid rgba(0,0,0,0.08); flex-shrink: 0; }
    .swatch-name { font-size: 0.82rem; font-weight: 600; color: var(--heading); }
    .swatch-hex { font-size: 0.76rem; color: var(--muted); font-family: ui-monospace, monospace; }
    .mock-shell { padding: 20px; background: var(--bg); }
    .mock-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
      flex-wrap: wrap;
      background: var(--surface);
      border: 1px solid var(--border);
      border-top: 3px solid var(--indigo);
      border-radius: 6px;
      padding: 12px 16px;
      margin-bottom: 16px;
    }
    .mock-tabs {
      display: flex;
      gap: 14px;
      border-bottom: 1px solid var(--border);
      margin-bottom: 16px;
      font-size: 0.85rem;
    }
    .mock-tab { padding: 7px 6px; color: var(--muted); border-bottom: 2px solid transparent; }
    .mock-tab.active { color: var(--indigo); font-weight: 600; border-bottom-color: var(--indigo); }
    .mock-btn {
      background: var(--indigo);
      color: #FFFFFF;
      border: 1px solid var(--indigo);
      border-radius: 4px;
      padding: 5px 11px;
      font-size: 0.8rem;
      font-weight: 500;
    }
    .mock-btn-outline {
      background: transparent;
      color: var(--indigo);
      border: 1px solid var(--indigo);
      border-radius: 4px;
      padding: 5px 11px;
      font-size: 0.8rem;
      font-weight: 500;
    }
    .mock-badge {
      display: inline-block;
      background: var(--indigo-soft);
      color: var(--indigo);
      border: 1px solid #C7CEE8;
      border-radius: 4px;
      padding: 2px 8px;
      font-size: 0.78rem;
      font-weight: 500;
    }
    .metrics {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 14px;
      margin-bottom: 24px;
    }
    .metric {
      background: var(--surface);
      border: 1px solid var(--border);
      border-top: 3px solid var(--indigo);
      border-radius: 8px;
      padding: 16px;
    }
    .metric-label { font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.04em; color: var(--muted); }
    .metric-val { font-size: 1.65rem; font-weight: 700; color: var(--heading); margin-top: 4px; font-variant-numeric: tabular-nums; }
    .cmd-box {
      background: #1C234A;
      color: #F6F7FB;
      border-radius: 8px;
      padding: 16px 20px;
      margin-bottom: 24px;
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 0.85rem;
      overflow-x: auto;
    }
    .cmd-box .cmd-head { color: #A5B0D6; font-size: 0.75rem; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.05em; }
    .cmd-box code { display: block; margin: 3px 0; }
    .toolbar { display: flex; gap: 10px; align-items: center; flex-wrap: wrap; margin-bottom: 20px; }
    .filter-btn {
      border: 1px solid var(--border);
      background: var(--surface);
      color: var(--ink);
      padding: 7px 14px;
      border-radius: 6px;
      font-size: 0.85rem;
      font-weight: 600;
      cursor: pointer;
    }
    .filter-btn.active { background: var(--indigo); color: #FFFFFF; border-color: var(--indigo); }
    .search-input {
      margin-left: auto;
      min-width: 240px;
      padding: 7px 12px;
      border: 1px solid var(--border);
      border-radius: 6px;
      font-size: 0.88rem;
      background: var(--surface);
    }
    .suite-card { background: var(--surface); border: 1px solid var(--border); border-radius: 8px; margin-bottom: 18px; overflow: hidden; }
    .suite-header {
      padding: 16px 20px;
      background: #FAFBFD;
      border-bottom: 1px solid var(--border);
      display: flex;
      justify-content: space-between;
      align-items: flex-start;
      gap: 16px;
      flex-wrap: wrap;
    }
    .suite-header h3 { margin: 6px 0 4px; font-size: 1.02rem; font-family: ui-monospace, monospace; color: var(--heading); }
    .covers { margin: 0; font-size: 0.84rem; color: var(--muted); }
    .suite-meta { font-size: 0.84rem; color: var(--muted); white-space: nowrap; }
    .tier-tag {
      display: inline-block;
      font-size: 0.74rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      padding: 2px 8px;
      border-radius: 4px;
    }
    .tier-unit { background: var(--indigo-soft); color: var(--indigo); }
    .tier-server { background: var(--accent-soft); color: var(--accent); }
    .tier-e2e { background: #FFFBEB; color: var(--amber); }
    .case-list { margin: 0; padding: 12px 20px 16px 36px; }
    .case-item { padding: 8px 0; border-bottom: 1px solid #F3F4F8; }
    .case-item:last-child { border-bottom: none; }
    .case-title-row { display: flex; justify-content: space-between; gap: 12px; }
    .case-title { font-weight: 600; font-size: 0.92rem; }
    .case-line { font-family: ui-monospace, monospace; font-size: 0.78rem; color: var(--muted); }
    .case-desc { margin: 4px 0 0; font-size: 0.85rem; color: var(--muted); }
    table {
      width: 100%;
      border-collapse: collapse;
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      overflow: hidden;
      font-size: 0.88rem;
    }
    th, td { padding: 10px 14px; border-bottom: 1px solid var(--border); text-align: left; }
    th { background: var(--subtle); font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.04em; color: var(--heading); }
    td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
    .nocov-note { color: var(--amber); font-weight: 600; font-size: 0.82rem; }
  </style>
</head>
<body>
  <header class="topbar">
    <div class="topbar-inner">
      <div class="brand-wrap">
        <img src="/assets/app-logo" alt="CodingManageR Logo" />
        <div class="brand">CodingManageR</div>
      </div>
      <nav class="nav-tabs" aria-label="Ansichten">
        <button type="button" class="nav-tab active" data-tab="brand">Logo &amp; Indigo-Theme</button>
        <button type="button" class="nav-tab" data-tab="tests">Test Suite &amp; Coverage</button>
      </nav>
    </div>
  </header>

  <main class="wrap">
    <section id="panel-brand" class="panel active">
      <div class="hero">
        <div>
          <h1>CodingManageR · Finales Indigo-Logo &amp; Farbschema</h1>
          <p class="subtitle">
            Motiv aus Entwurf 2 (Team-Konsens &amp; Reliabilität) in reinem <strong>Scholarly Indigo (<code>#33418F</code>)</strong> mit freigestelltem transparentem Außenbereich — eingebunden in Shiny-App, <code>README</code> und <code>pkgdown</code>.
          </p>
        </div>
        <div class="ref-box">
          <img src="/assets/reference" alt="Vorlage iotarelr" />
          <div>
            <div class="ref-label">Stil-Vorlage</div>
            <div class="ref-title">iotarelr</div>
          </div>
        </div>
      </div>

      <div class="showcase-grid">
        <article class="card">
          <div class="card-head">
            <h3>Freigestelltes Hex-Logo (PNG mit Transparenz)</h3>
            <div class="bg-switcher">
              <button type="button" class="bg-btn active" data-stage="stage-light">Weiß</button>
              <button type="button" class="bg-btn" data-stage="stage-subtle">Hellgrau</button>
              <button type="button" class="bg-btn" data-stage="stage-dark">Dunkel</button>
            </div>
          </div>
          <div class="logo-stage stage-light">
            <img src="/assets/logo" alt="Finales CodingManageR Indigo Hex-Logo" />
          </div>
          <div class="card-body">
            <div class="scale-strip">
              <div class="scale-item">
                <img src="/assets/logo" alt="" height="110" />
                <span>README (139px)</span>
              </div>
              <div class="scale-item">
                <img src="/assets/app-logo" alt="" height="38" />
                <span>App-Header (34–38px)</span>
              </div>
              <div class="scale-item">
                <img src="/assets/favicon" alt="" width="32" height="32" />
                <span>Favicon (32px)</span>
              </div>
              <div class="scale-item">
                <img src="/assets/favicon" alt="" width="16" height="16" />
                <span>Tab (16px)</span>
              </div>
            </div>
          </div>
        </article>

        <article class="card">
          <div class="card-head">
            <h3>Scholarly Indigo · App- &amp; pkgdown-Tokens</h3>
          </div>
          <div class="card-body" style="border-top:none;">
            <div class="swatches">
              <div class="swatch"><span class="swatch-box" style="background:#33418F"></span><div><div class="swatch-name">Primary Indigo</div><div class="swatch-hex">#33418F</div></div></div>
              <div class="swatch"><span class="swatch-box" style="background:#26316E"></span><div><div class="swatch-name">Hover / Active</div><div class="swatch-hex">#26316E</div></div></div>
              <div class="swatch"><span class="swatch-box" style="background:#1C234A"></span><div><div class="swatch-name">Heading Ink</div><div class="swatch-hex">#1C234A</div></div></div>
              <div class="swatch"><span class="swatch-box" style="background:#EEF0FA"></span><div><div class="swatch-name">Soft Tint</div><div class="swatch-hex">#EEF0FA</div></div></div>
              <div class="swatch"><span class="swatch-box" style="background:#F6F7FB"></span><div><div class="swatch-name">Canvas</div><div class="swatch-hex">#F6F7FB</div></div></div>
              <div class="swatch"><span class="swatch-box" style="background:#525B70"></span><div><div class="swatch-name">Muted Slate</div><div class="swatch-hex">#525B70</div></div></div>
            </div>
          </div>
          <div class="mock-shell">
            <div class="mock-header">
              <div class="brand-wrap">
                <img src="/assets/app-logo" alt="" style="height:30px;" />
                <span class="brand" style="font-size:0.98rem;">CodingManageR</span>
              </div>
              <div style="display:flex;gap:8px;">
                <span class="mock-btn-outline">Tour starten</span>
                <span class="mock-btn">Speichern</span>
              </div>
            </div>
            <div class="mock-tabs">
              <span class="mock-tab active">Dashboard</span>
              <span class="mock-tab">Kodier-Training</span>
              <span class="mock-tab">Live-Kodierung</span>
              <span class="mock-tab">Admin</span>
            </div>
            <div style="display:flex;gap:8px;align-items:center;flex-wrap:wrap;">
              <span class="mock-badge">S1_Cognitive_Activation</span>
              <span class="mock-badge">Klieme2024</span>
              <span style="font-size:0.8rem;color:var(--muted);">KI-Hinweis aktiv in allen 34 R-Skripten, README, 2 Vignetten &amp; Demo-Daten</span>
            </div>
          </div>
        </article>
      </div>
    </section>

    <section id="panel-tests" class="panel">
      <div class="hero">
        <div>
          <h1>CodingManageR · Automated Test Suite &amp; Coverage Explorer</h1>
          <p class="subtitle">
            Three-tier R/Shiny verification suite built with <code>testthat</code> (edition 3), <code>shiny::testServer()</code>, and programmatic <code>shinytest2::AppDriver</code> E2E tests (SQLite backend, MySQL/MariaDB branches excluded via <code># nocov</code>, target &ge; 95% coverage).
          </p>
        </div>
      </div>

      <div class="metrics">
        <div class="metric">
          <div class="metric-label">Test Files</div>
          <div class="metric-val">${suites.length}</div>
        </div>
        <div class="metric">
          <div class="metric-label">test_that() Suites</div>
          <div class="metric-val">${totalCases}</div>
        </div>
        <div class="metric">
          <div class="metric-label">Test Code Lines</div>
          <div class="metric-val">${totalTestLines}</div>
        </div>
        <div class="metric">
          <div class="metric-label">Coverage Target</div>
          <div class="metric-val">&ge; 95.0%</div>
        </div>
        <div class="metric">
          <div class="metric-label">MySQL # nocov Lines</div>
          <div class="metric-val">${totalNocovLines}</div>
        </div>
      </div>

      <div class="cmd-box">
        <div class="cmd-head">Run Locally in R (testthat + shinytest2 + covr)</div>
        <code>Rscript tests/run_coverage.R</code>
        <code>Rscript -e "devtools::test()"</code>
        <code>Rscript -e "covr::report(covr::package_coverage(type = 'tests'))"</code>
      </div>

      <div class="toolbar">
        <button class="filter-btn active" data-filter="all">All Tiers (${suites.length})</button>
        <button class="filter-btn" data-filter="unit">Tier 1 · Unit &amp; SQLite</button>
        <button class="filter-btn" data-filter="server">Tier 2 · shiny::testServer</button>
        <button class="filter-btn" data-filter="e2e">Tier 3 · shinytest2 E2E</button>
        <input id="searchBox" class="search-input" type="search" placeholder="Filter tests by keyword..." />
      </div>

      <div id="suiteContainer">${suiteCardsHtml}</div>

      <h2>R Package Source Files &amp; <code># nocov</code> Exclusions</h2>
      <table>
        <thead>
          <tr>
            <th>Source File</th>
            <th class="num">Total Lines</th>
            <th class="num"># nocov Lines</th>
          </tr>
        </thead>
        <tbody>${sourceRowsHtml}</tbody>
      </table>
    </section>
  </main>

  <script>
    const navTabs = document.querySelectorAll('.nav-tab');
    const panels = {
      brand: document.getElementById('panel-brand'),
      tests: document.getElementById('panel-tests')
    };

    navTabs.forEach((tab) => {
      tab.addEventListener('click', () => {
        navTabs.forEach((t) => t.classList.remove('active'));
        tab.classList.add('active');
        const target = tab.dataset.tab;
        Object.entries(panels).forEach(([key, el]) => {
          el.classList.toggle('active', key === target);
        });
      });
    });

    const bgBtns = document.querySelectorAll('.bg-btn');
    const stages = document.querySelectorAll('.logo-stage');
    bgBtns.forEach((btn) => {
      btn.addEventListener('click', () => {
        bgBtns.forEach((b) => b.classList.remove('active'));
        btn.classList.add('active');
        const cls = btn.dataset.stage;
        stages.forEach((s) => {
          s.classList.remove('stage-light', 'stage-subtle', 'stage-dark');
          s.classList.add(cls);
        });
      });
    });

    const buttons = document.querySelectorAll('.filter-btn');
    const cards = document.querySelectorAll('.suite-card');
    const searchBox = document.getElementById('searchBox');
    let activeTier = 'all';

    function applyFilters() {
      const q = (searchBox.value || '').trim().toLowerCase();
      cards.forEach((card) => {
        const tierOk = activeTier === 'all' || card.dataset.tier === activeTier;
        const textOk = !q || card.textContent.toLowerCase().includes(q);
        card.style.display = tierOk && textOk ? '' : 'none';
      });
    }

    buttons.forEach((btn) => {
      btn.addEventListener('click', () => {
        buttons.forEach((b) => b.classList.remove('active'));
        btn.classList.add('active');
        activeTier = btn.dataset.filter;
        applyFilters();
      });
    });

    searchBox.addEventListener('input', applyFilters);
  </script>
</body>
</html>`;
}

function serveFile(res, filePath, contentType) {
  if (!fs.existsSync(filePath)) {
    res.writeHead(404, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Not found');
    return;
  }
  const data = fs.readFileSync(filePath);
  res.writeHead(200, {
    'Content-Type': contentType,
    'Cache-Control': 'no-cache'
  });
  res.end(data);
}

const server = http.createServer((req, res) => {
  const urlPath = (req.url || '/').split('?')[0];

  if (urlPath === '/api/suites') {
    res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
    res.end(JSON.stringify({ suites: parseTestFiles(), sources: parseRSourceSummary() }, null, 2));
    return;
  }

  if (urlPath === '/assets/logo') {
    serveFile(res, FINAL_ASSETS.logo.file, FINAL_ASSETS.logo.mime);
    return;
  }
  if (urlPath === '/assets/app-logo') {
    serveFile(res, FINAL_ASSETS.appLogo.file, FINAL_ASSETS.appLogo.mime);
    return;
  }
  if (urlPath === '/assets/favicon') {
    serveFile(res, FINAL_ASSETS.favicon.file, FINAL_ASSETS.favicon.mime);
    return;
  }
  if (urlPath === '/assets/reference') {
    serveFile(res, FINAL_ASSETS.reference.file, FINAL_ASSETS.reference.mime);
    return;
  }

  res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
  res.end(buildHtml());
});

server.listen(PORT, HOST, () => {
  console.log(`[CodingManageR Preview] Listening on http://${HOST}:${PORT}`);
});
