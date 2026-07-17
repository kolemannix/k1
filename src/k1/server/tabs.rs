use super::DisplayFn;
use maud::html;
use std::fmt;

/// Tab buttons driven by one datastar signal, one pane per tab. `nav()` renders
/// the buttons; gate each pane with `data-show=(tabs.selected(key))`. The first
/// tab is the default.
pub struct Tabs {
    signal: String,
    tabs: Vec<TabDef>,
}

struct TabDef {
    key: &'static str,
    label: &'static str,
    /// Extra datastar expression run when the tab is selected, e.g. a lazy @get
    on_select: Option<String>,
}

impl Tabs {
    pub fn new(signal: String) -> Tabs {
        Tabs { signal, tabs: vec![] }
    }

    pub fn tab(self, key: &'static str, label: &'static str) -> Tabs {
        self.add(TabDef { key, label, on_select: None })
    }

    pub fn tab_with_action(self, key: &'static str, label: &'static str, action: String) -> Tabs {
        self.add(TabDef { key, label, on_select: Some(action) })
    }

    fn add(mut self, tab: TabDef) -> Tabs {
        self.tabs.push(tab);
        self
    }

    /// Expression testing whether `key` is the selected tab
    pub fn selected(&self, key: &'static str) -> impl fmt::Display + '_ {
        debug_assert!(self.tabs.iter().any(|t| t.key == key), "unknown tab '{key}'");
        DisplayFn(move |f| write!(f, "${} == '{}'", self.signal, key))
    }

    pub fn nav(&self) -> maud::Markup {
        let default = self.tabs.first().expect("no tabs").key;
        html! {
            nav .result-tabs
                data-signals__ifmissing=(format!("{{{}: '{}'}}", self.signal, default)) {
                @for tab in &self.tabs {
                    @let select = format!("${} = '{}'", self.signal, tab.key);
                    @let on_click = match &tab.on_select {
                        None => select,
                        Some(action) => format!("{select}; {action}"),
                    };
                    button .result-tab
                        data-class:active=(self.selected(tab.key))
                        data-on:click=(on_click)
                        { (tab.label) }
                }
            }
        }
    }
}
