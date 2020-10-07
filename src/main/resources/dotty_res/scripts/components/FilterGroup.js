class FilterGroup extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isVisible: false,
    };

    this.filterToggleRef = findRef(".filterToggleButton");
    this.filterLowerContainerRef = findRef(".filterLowerContainer");

    this.onClickFn = withEvent(
      this.filterToggleRef,
      "click",
      this.props.onFilterVisibilityChange
    );

    this.render(this.props);
  }

  isActive(isActive) {
    return isActive ? "active" : "";
  }

  getFilterGroup(title, values) {
    return `
      <div class="filterGroup">
        <span class="groupTitle">${title.substring(1)}</span>
        <div class="filterList">
          ${Object.entries(values).map(
            ([key, isActive]) =>
              `<button class="filterButtonItem ${this.isActive(
                isActive
              )}" data-key="${title}" data-value="${key}">${key}</button>`
          )}
        </div>
      </div>
    `;
  }

  onFilterClick = ({
    currentTarget: {
      dataset: { key, value },
    },
  }) => {
    this.props.onFilterToggle(key, value);
  };

  attachFiltersClicks() {
    [
      ...findRefs("button.filterButtonItem", this.filterLowerContainerRef),
    ].map((buttonRef) => withEvent(buttonRef, "click", this.onFilterClick));
  }

  render({ groups }) {
    attachDOM(
      this.filterLowerContainerRef,
      Object.entries(groups).map(([key, values]) =>
        this.getFilterGroup(key, values)
      )
    );
    this.attachFiltersClicks();
  }
}
