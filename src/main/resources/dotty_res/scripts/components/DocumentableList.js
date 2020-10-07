class DocumentableList extends Component {
  constructor(props) {
    super(props);

    this.togglableRefs = [...findRefs(".tabbedcontent div[data-togglable]")];
    this.render(this.props);
  }

  filterLists = (inputValue) => (togglableRef) => {
    return [...findRefs(".documentableList", togglableRef)].map((listRef) => {
      const visibleChildren = this.filterElements(listRef, inputValue);
      ifVisible(visibleChildren.length, listRef, "block");
    });
  };

  filterElements = (listRef, inputValue) => {
    return [...findRefs(".documentableElement", listRef)]
      .map((elementRef) => ({
        ref: elementRef,
        name: getText(findRef(".documentableName", elementRef)),
        description: getText(findRef(".documentableBrief", elementRef)),
      }))
      .filter(({ ref, ...data }) => {
        const includes = this.includesInputValue(data, inputValue);
        ifVisible(includes, ref, "table");
        toggleVisibility(includes, ref);
        return includes;
      });
  };

  includesInputValue = ({ name, description }, inputValue) => {
    return name.includes(inputValue) || description.includes(inputValue);
  };

  render({ value }) {
    this.togglableRefs.map(this.filterLists(value));
  }
}
