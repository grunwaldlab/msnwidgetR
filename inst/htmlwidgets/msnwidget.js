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

        widget = window.msnwidget.msnWidget(
          el,
          x.datasets || x.dataset || [],
          { ...(x.options || {}), offline: true, width: width, height: height }
        );
      },

      resize: function (width, height) {
        // The widget's ResizeObserver handles layout changes.
      }
    };
  }
});
