const sidebars = {
  sidebar: [
    "index",
    "getting-started",
    "printing",
    {
      type: "category",
      label: "Reference",
      collapsed: false,
      items: [
        "syntax",
        "regex",
        "errors",
        "reference-parser",
        "reference-printer"
      ]
    },
    {
      type: "category",
      label: "Tutorials",
      collapsed: true,
      items: ["tutorial", "example-json", "example-caliban"]
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: true,
      items: [
        "how-to-whitespace-and-comments",
        "how-to-recursive-syntaxes",
        "how-to-error-messages",
        "how-to-stream-printing",
        "how-to-performance"
      ]
    },
    "resources"
  ]
};

module.exports = sidebars;
