const defaultFilterGroup = {
  FOrdering: { Alphabetical: true },
};

class FilterBar extends Component {
  constructor(props) {
    super(props);

    this.state = {
      value: "",
      filters: this.generateGroups(),
      isVisible: false,
    };

    this.filterBarRef = findRef(".documentableFilter");

    this.inputComp = new Input({ onInputChange: this.onInputChange });
    this.documentableList = new DocumentableList({
      value: this.state.value,
    });
    this.filterGroupComp = new FilterGroup({
      groups: this.state.filters,
      onFilterToggle: this.onFilterToggle,
      onFilterVisibilityChange: this.onFilterVisibilityChange,
    });
  }

  onInputChange = (value) => {
    this.setState({ value, filters: this.generateGroups() }, () => {
      this.documentableList.render({ value: this.state.value });
      this.filterGroupComp.render({ groups: this.state.filters });
    });
  };

  onFilterVisibilityChange = () => {
    this.setState((prevState) => ({ isVisible: !prevState.isVisible }));
  };

  onFilterToggle = (key, value) => {
    this.setState(
      (prevState) => ({
        filters: {
          ...prevState.filters,
          [key]: {
            ...prevState.filters[key],
            [value]: !prevState.filters[key][value],
          },
        },
      }),
      () => {
        this.documentableList.render({ value: this.state.value });
        this.filterGroupComp.render({ groups: this.state.filters });
      }
    );
  };

  generateGroups() {
    return {
      ...defaultFilterGroup,
      ...[...findRefs(".documentableElement")].reduce(
        this.getGroupFromDataset,
        {}
      ),
    };
  }

  getGroupFromDataset(group, { dataset }) {
    if (dataset.visibility === "true" || !dataset.visibility) {
      Object.entries(dataset).map(([key, value]) => {
        if (!startsWith(key, "f")) {
          return;
        }
        if (!group[key]) {
          group[key] = { [value]: true };
        } else {
          group[key] = {
            ...group[key],
            [value]: true,
          };
        }
      });
    }
    return group;
  }

  render() {
    const { isVisible } = this.state;

    if (this.filterBarRef) {
      if (isVisible) {
        this.filterBarRef.classList.add("active");
      } else {
        this.filterBarRef.classList.remove("active");
      }
    }
  }
}

init(() => new FilterBar());
