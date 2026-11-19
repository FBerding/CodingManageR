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
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>CodingManageR · Test Suite &amp; Coverage Explorer</title>
  <style>
    :root {
      --bg: #f6f7f9;
      --surface: #ffffff;
      --ink: #111827;
      --muted: #4b5563;
      --border: #e5e7eb;
      --accent: #0f766e;
      --accent-soft: #ecfdf5;
      --blue: #1d4ed8;
      --amber: #b45309;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      background: var(--bg);
      color: var(--ink);
      line-height: 1.5;
    }
    .wrap { max-width: 1120px; margin: 0 auto; padding: 32px 24px 64px; }
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
    h1 { margin: 0 0 6px; font-size: 1.75rem; letter-spacing: -0.02em; }
    .subtitle { margin: 0; color: var(--muted); max-width: 68ch; font-size: 0.95rem; }
    .metrics {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 14px;
      margin-bottom: 24px;
    }
    .metric {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 16px;
    }
    .metric-label { font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.04em; color: var(--muted); }
    .metric-val { font-size: 1.65rem; font-weight: 700; margin-top: 4px; font-variant-numeric: tabular-nums; }
    .cmd-box {
      background: #111827;
      color: #f9fafb;
      border-radius: 8px;
      padding: 16px 20px;
      margin-bottom: 24px;
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 0.85rem;
      overflow-x: auto;
    }
    .cmd-box .cmd-head { color: #9ca3af; font-size: 0.75rem; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.05em; }
    .cmd-box code { display: block; margin: 3px 0; }
    .toolbar {
      display: flex;
      gap: 10px;
      align-items: center;
      flex-wrap: wrap;
      margin-bottom: 20px;
    }
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
    .filter-btn.active {
      background: var(--ink);
      color: #fff;
      border-color: var(--ink);
    }
    .search-input {
      margin-left: auto;
      min-width: 240px;
      padding: 7px 12px;
      border: 1px solid var(--border);
      border-radius: 6px;
      font-size: 0.88rem;
      background: var(--surface);
    }
    .suite-card {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      margin-bottom: 18px;
      overflow: hidden;
    }
    .suite-header {
      padding: 16px 20px;
      background: #fafafa;
      border-bottom: 1px solid var(--border);
      display: flex;
      justify-content: space-between;
      align-items: flex-start;
      gap: 16px;
      flex-wrap: wrap;
    }
    .suite-header h3 { margin: 6px 0 4px; font-size: 1.02rem; font-family: ui-monospace, monospace; }
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
    .tier-unit { background: #eff6ff; color: var(--blue); }
    .tier-server { background: var(--accent-soft); color: var(--accent); }
    .tier-e2e { background: #fef3c7; color: var(--amber); }
    .case-list { margin: 0; padding: 12px 20px 16px 36px; }
    .case-item { padding: 8px 0; border-bottom: 1px solid #f3f4f6; }
    .case-item:last-child { border-bottom: none; }
    .case-title-row { display: flex; justify-content: space-between; gap: 12px; }
    .case-title { font-weight: 600; font-size: 0.92rem; }
    .case-line { font-family: ui-monospace, monospace; font-size: 0.78rem; color: var(--muted); }
    .case-desc { margin: 4px 0 0; font-size: 0.85rem; color: var(--muted); }
    h2 { margin: 32px 0 12px; font-size: 1.2rem; }
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
    th { background: #fafafa; font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.04em; color: var(--muted); }
    td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
    .nocov-note { color: var(--amber); font-weight: 600; font-size: 0.82rem; }
  </style>
</head>
<body>
  <div class="wrap">
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
  </div>

  <script>
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

const server = http.createServer((req, res) => {
  if (req.url === '/api/suites') {
    res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
    res.end(JSON.stringify({ suites: parseTestFiles(), sources: parseRSourceSummary() }, null, 2));
    return;
  }

  res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
  res.end(buildHtml());
});

server.listen(PORT, HOST, () => {
  console.log(`[CodingManageR Preview] Listening on http://${HOST}:${PORT}`);
});
