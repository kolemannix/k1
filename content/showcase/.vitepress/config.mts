import { readdirSync, readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vitepress'

const root = fileURLToPath(new URL('..', import.meta.url))
const k1Grammar = JSON.parse(readFileSync(`${root}../../tools/vscode-k1/syntaxes/k1.tmLanguage.json`, 'utf8'))

function page(path: string) {
  const title = readFileSync(`${root}${path}.md`, 'utf8').match(/^# (.+)$/m)![1]
  return { text: title.replace(/`([^`]+)`/g, '<code>$1</code>'), link: `/${path}` }
}

const sections = []
for (const file of readdirSync(`${root}sections`).sort()) {
  sections.push(page(`sections/${file.replace(/\.md$/, '')}`))
}

export default defineConfig({
  title: 'K1 showcase',
  rewrites: { 'README.md': 'index.md' },
  srcExclude: ['examples/**', '**/gen/**'],
  markdown: { languages: [k1Grammar] },
  themeConfig: {
    outline: [2, 3],
    search: { provider: 'local' },
    editLink: { pattern: `vscode://file${root}:path`, text: 'Open in editor' },
    sidebar: [
      { text: 'Introduction', link: '/' },
      { text: 'Sections', items: sections },
      {
        text: 'Benchmarks',
        items: [
          page('benchmarks/runtime/results'),
          page('benchmarks/compile-times/results'),
          page('benchmarks/comptime/results'),
          page('benchmarks/brotli/results'),
          page('benchmarks/brotli'),
        ],
      },
    ],
  },
})
