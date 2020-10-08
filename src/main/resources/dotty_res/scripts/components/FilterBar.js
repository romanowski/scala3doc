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
      filters: this.state.filters,
    });
    this.filterGroupComp = new FilterGroup({
      groups: this.state.filters,
      onFilterToggle: this.onFilterToggle,
      onGroupSelectChange: this.onGroupSelectChange,
      onFilterVisibilityChange: this.onFilterVisibilityChange,
    });
  }

  onInputChange = (value) => {
    this.setState(
      (prevState) => ({
        value,
        filters: this.generateGroups(prevState.filters),
      }),
      () => {
        this.documentableList.render({
          value: this.state.value,
          filters: this.state.filters,
        });
        this.filterGroupComp.render({ groups: this.state.filters });
      }
    );
  };

  onGroupSelectChange = (key, isActive) => {
    this.setState(
      (prevState) => ({
        filters: {
          ...prevState.filters,
          [key]: Object.keys(prevState.filters[key]).reduce(
            (obj, key) => ((obj[key] = isActive), obj),
            {}
          ),
        },
      }),
      () => {
        this.documentableList.render({
          value: this.state.value,
          filters: this.state.filters,
        });
        this.filterGroupComp.render({ groups: this.state.filters });
      }
    );
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
        this.documentableList.render({
          value: this.state.value,
          filters: this.state.filters,
        });
        this.filterGroupComp.render({ groups: this.state.filters });
      }
    );
  };

  generateGroups(initial = {}) {
    return [...findRefs(".documentableElement")].reduce(
      this.getGroupFromDataset,
      initial
    );
  }

  getGroupFromDataset(group, { dataset }) {
    Object.entries(dataset).map(([key, value]) => {
      if (!startsWith(key, "f")) {
        return;
      }
      if (!group[key]) {
        group[key] = { [value]: true };
      } else {
        group[key] = {
          ...group[key],
          [value]: group[key][value] ?? true,
        };
      }
    });
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
