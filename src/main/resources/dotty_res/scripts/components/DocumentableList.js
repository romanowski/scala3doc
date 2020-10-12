const superTypesDataset = "Linear supertypes";

class DocumentableList extends Component {
  constructor(props) {
    super(props);

    this.refs = {
      tabs: findRefs(".section-tab[data-togglable]", findRef(".tabbedcontent")),
      sections: findRefs("div[data-togglable]", findRef(".tabbedcontent")),
    };

    this.state = {
      list: new List(this.refs.tabs, this.refs.sections),
    };

    this.render(this.props);
  }

  toggleElementDatasetVisibility(condition, element) {
    if (condition) {
      element.dataset.visibility = true;
    } else {
      element.dataset.visibility = false;
    }
  }

  toggleDisplayStyles(condition, ref, onVisibleStyle) {
    if (condition) {
      ref.style.display = onVisibleStyle;
    } else {
      ref.style.display = "none";
    }
  }

  render({ filter }) {
    this.state.list.sectionsRefs.map((sectionRef) => {
      const tabRef = this.state.list.getTabRefFromSectionRef(sectionRef);

      const isTabVisible = this.state.list
        .getSectionListRefs(sectionRef)
        .filter((listRef) => {
          const isListVisible = this.state.list
            .getSectionListElementsRefs(listRef)
            .map((elementRef) => this.state.list.mapListElementRef(elementRef))
            .filter((elementData) => {
              const isElementVisible = this.state.list.isElementVisible(
                elementData,
                filter
              );

              this.toggleDisplayStyles(
                isElementVisible,
                elementData.ref,
                "table"
              );
              this.toggleElementDatasetVisibility(
                isElementVisible,
                elementData.ref
              );
              return isElementVisible;
            }).length;

          this.toggleDisplayStyles(isListVisible, listRef, "block");

          return isListVisible;
        }).length;

      this.toggleDisplayStyles(isTabVisible, tabRef, "inline-block");
    });
  }
}

class List {
  _linearTab = "Linear supertypes";

  constructor(tabsRef, sectionRefs) {
    this._tabsRef = tabsRef;
    this._sectionRefs = sectionRefs;
  }

  get tabsRefs() {
    return this._tabsRef.filter(
      (tabRef) => this._getTogglable(tabRef) !== this._linearTab
    );
  }

  get sectionsRefs() {
    return this._sectionRefs.filter(
      (sectionRef) => this._getTogglable(sectionRef) !== this._linearTab
    );
  }

  getTabRefFromSectionRef(sectionRef) {
    return this.tabsRefs.find(
      (tabRef) => this._getTogglable(tabRef) === this._getTogglable(sectionRef)
    );
  }

  getSectionListRefs(sectionRef) {
    return findRefs(".documentableList", sectionRef);
  }

  getSectionListElementsRefs(listRef) {
    return findRefs(".documentableElement", listRef);
  }

  mapListElementRef(elementRef) {
    return {
      ref: elementRef,
      name: getElementTextContent(getElementNameRef(elementRef)),
      description: getElementTextContent(getElementDescription(elementRef)),
    };
  }

  isElementVisible(elementData, filter) {
    if (!this._anyFilterFromElementSelected(elementData, filter)) {
      return false;
    }
    return this._includesInputValue(elementData, filter);
  }

  _includesInputValue = (elementData, filter) => {
    if (elementData.name.includes(filter.value)) {
      return true;
    }
    return elementData.description.includes(filter.value);
  };

  _anyFilterFromElementSelected(elementRef, filter) {
    const dataset = this._getCorrectDatasetFromElement(elementRef);
    return dataset.length
      ? this._hasCorrespondingFilter(dataset, filter.filters)
      : true;
  }

  _hasCorrespondingFilter = (dataset, filters) =>
    dataset.some(([key, value]) => {
      const filterGroup = filters[key];
      return this._splitByComma(value).some(
        (val) => filterGroup && filterGroup[val].selected
      );
    });

  _getCorrectDatasetFromElement = (elementRef) =>
    Object.entries(elementRef.ref.dataset).filter(([key]) =>
      this._startsWithF(key)
    );

  _splitByComma = (str) => str.split(",");

  _startsWithF = (str) => startsWith(str, "f");

  _getTogglable = (elementRef) => elementRef.dataset.togglable;
}
