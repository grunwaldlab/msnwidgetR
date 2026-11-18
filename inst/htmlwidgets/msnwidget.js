HTMLWidgets.widget({
  name: "msnwidget",

  type: "output",

  factory: function (el, width, height) {
    let widget;

    return {
      renderValue: function (x) {
        if (widget) {
          widget.destroy();
        }
        const options = x.options || {};
        delete x.options;

        widget = window.msnwidget.msnWidget(el, x, options);
        // widget = window.msnwidget.msnWidget(el, { name: x.name, x.data, kind: x.kind, x.metadata }, { offline: true, x.options })
      },

      resize: function (width, height) {
        // The widget's ResizeObserver handles layout changes.
      }
    };
  }
});
