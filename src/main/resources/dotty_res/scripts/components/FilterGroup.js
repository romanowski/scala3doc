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
        <div class="groupTitle">
          <span>${title.substring(1)}</span>
          <div class="groupButtonsContainer">
            <button class="selectAll" data-key="${title}">Select All</button>
            <button class="deselectAll" data-key="${title}">Deselect All</button>
          </div>
        </div>
        <div class="filterList">
          ${Object.entries(values)
            .sort((a, b) => a[0].localeCompare(b[0]))
            .map(
              ([key, isActive]) =>
                `<button class="filterButtonItem ${this.isActive(
                  isActive
                )}" data-key="${title}" data-value="${key}">${key}</button>`
            )
            .join(" ")}
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

  onSelectAllClick = ({
    currentTarget: {
      dataset: { key },
    },
  }) => {
    this.props.onGroupSelectChange(key, true);
  };

  onDeselectAllClick = ({
    currentTarget: {
      dataset: { key },
    },
  }) => {
    this.props.onGroupSelectChange(key, false);
  };

  attachFiltersClicks() {
    [
      ...findRefs("button.filterButtonItem", this.filterLowerContainerRef),
    ].map((buttonRef) => withEvent(buttonRef, "click", this.onFilterClick));
  }

  attachSelectingButtonsClicks() {
    [
      ...findRefs("button.selectAll", this.filterLowerContainerRef),
    ].map((selectAllRef) =>
      withEvent(selectAllRef, "click", this.onSelectAllClick)
    );

    [
      ...findRefs("button.deselectAll", this.filterLowerContainerRef),
    ].map((selectAllRef) =>
      withEvent(selectAllRef, "click", this.onDeselectAllClick)
    );
  }

  render({ groups }) {
    attachDOM(
      this.filterLowerContainerRef,
      Object.entries(groups).map(([key, values]) =>
        this.getFilterGroup(key, values)
      )
    );
    this.attachFiltersClicks();
    this.attachSelectingButtonsClicks();
  }
}
