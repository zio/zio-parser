const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Parser",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        {
          type: "category",
          label: "Tutorial",
          collapsed: false,
          items: ["tutorial"]
        },
        {
          type: "category",
          label: "How-To Guides",
          collapsed: false,
          items: [
            "how-to-whitespace-and-comments",
            "how-to-recursive-syntaxes",
            "how-to-error-messages",
            "how-to-stream-printing",
            "how-to-performance"
          ]
        },
        {
          type: "category",
          label: "Concepts",
          collapsed: false,
          items: ["printing"]
        },
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
          label: "Examples",
          collapsed: false,
          items: ["example-json", "example-caliban"]
        }
      ]
    }
  ]
};

module.exports = sidebars;
