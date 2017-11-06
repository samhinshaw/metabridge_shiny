//
// const data = {
//   columns: []
//   addColumns: function(column) {
//     this.columns.push({
//
//     })
//   }
// };
//
// const handlers = {
//   addColums: function( {
//     let columsPicked = document.getElementById("columnsPicked");
//     columns.
//   })
// };

// const lisToChange = document.getElementById("addTodoTextInput");
// const matchingElemtents = [];
// const helpNavBar = document.querySelectorAll('li > a[data-value="Help"]');
// const aboutNavBar = document.querySelectorAll('li > a[data-value="About"]');

// JQuery code to *effectively* move navbar elements to right! This works, and is probably the best way to do it in the future... HOWEVER, it causes problems with shiny knowing what tab you're on, as it's technically a separate list from the #navbarLayout list. Also, it has a pop-in, which is visually unappealing. This cannot be easily styled with CSS without messing with the responsiveness of the site overall.

// $(document).ready(function() {
//   $("#navbarLayout").after(
//     '<ul class="nav navbar-nav navbar-right shiny-tab-input shiny-bound-input" id="navbarLayoutRight"></ul>'
//   );
//   var aboutElement = $('li:has(> a[data-value^="About"])').detach();
//   var helpElement = $('li:has(> a[data-value^="Help"])').detach();
//   $("#navbarLayoutRight").append(aboutElement);
//   $("#navbarLayoutRight").append(helpElement);
// });
//
// Initialize Tooltips with JQuery
// $(document).ready(() => {
//   // Disable viz panel on page load
//   $("a[data-value='vizPanel']")
//     .parent()
//     .addClass('disabled');
//   // Add btn-tooltip class
//   $("a[data-value='vizPanel']").addClass('panel-tooltip');
//   // Add title class
//   $("a[data-value='vizPanel']").attr(
//     'title',
//     'Please select a metabolite that has been mapped via KEGG'
//   );
//   // Initialize JQuery tooltips (but we're not using them anymore!)
//   // $('[data-toggle="tooltip"]').tooltip();
// });

const handlers = {
  // we're passing a static object, so this will only run ONCE.
  // However, we could link this to button presses to make the loading even more dynamic
  lazyLoadPackages: num => {
    // First argument is input name
    // Second argument is value to send
    Shiny.onInputChange('sessionInitialized', num);
  },
  initGetStarted: () => {
    const getStartedButton = document.getElementById('getStarted');
    // change innerHTML
    getStartedButton.innerHTML = 'Get Started';
    // remove disabled class
    getStartedButton.classList.remove('disabled');
    // add btn-tooltip class
    getStartedButton.classList.add('btn-tooltip');
    // Add title
    getStartedButton.setAttribute('title', "Let's Go!");
    // activate tippy on this button!
    handlers.activateTooltips(['#getStarted']);
  },
  clearMappingTables: () => {
    const mappingTable = document.getElementById('mappedMetaboliteTable');
    mappingTable.innerHTML = '';
    const mappingSummaryTable = document.getElementById('mappingSummaryTable');
    mappingSummaryTable.innerHTML = '';
  },
  clearUploadedTable: () => {
    const uploadedDataTable = document.getElementById('uploadedDataTable');
    uploadedDataTable.innerHTML = '';
  },
  addToolTips: () => {
    // Disable viz panel on page load
    $("a[data-value='vizPanel']")
      .parent()
      .addClass('disabled');
    // Add tooltip class
    $("a[data-value='vizPanel']").addClass('panel-tooltip');
    // Add title class
    $("a[data-value='vizPanel']").attr(
      'title',
      'Please select a metabolite that has been mapped via KEGG'
    );
  },
  activateTooltips: selectors => {
    const tippyOptions = {
      size: 'big',
      duration: 150
    };
    selectors.map(selector => {
      tippy(selector, tippyOptions);
    });
  },
  passWindowInformation: () => {
    const windowWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    const windowHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    const navbarHeight = document.getElementsByClassName(
      'navbar navbar-default navbar-static-top'
    )[0].offsetHeight;
    const vizPanelTabHeaderHeight = $('#vizPanelUI .tab-header').outerHeight(true);

    // If the viz panel has been rendered...
    if (document.getElementById('vizPanelUI').getElementsByClassName('col-sm-9')[0]) {
      const vizPanelWidth =
        document.getElementById('vizPanelUI').getElementsByClassName('col-sm-9')[0].offsetWidth -
        15;

      // It's still easiest to use JQuery to get height WITH margin
      const vizPanelHeight = windowHeight - navbarHeight - vizPanelTabHeaderHeight - 25;

      Shiny.onInputChange('vizPanelHeight', vizPanelHeight);
      Shiny.onInputChange('vizPanelWidth', vizPanelWidth);
    }

    // First argument is input name
    // Second argument is value to send
    Shiny.onInputChange('windowWidth', windowWidth);
    Shiny.onInputChange('windowHeight', windowHeight);
    Shiny.onInputChange('navbarHeight', navbarHeight);
  }
};

// We must use shiny:sessioninitialized, not DOM Content Loaded
$(document).on('shiny:sessioninitialized', () => {
  handlers.lazyLoadPackages(1);
  handlers.addToolTips();
  handlers.activateTooltips(['.panel-tooltip', '.btn-tooltip']);
  // handlers.activateTooltips('.btn-tooltip');
  // Pass DOM sizing info to Shiny upon session initialization
  handlers.passWindowInformation();
});

window.onbeforeunload = function() {
  return 'Note: If you navigate away, you will lose all of your intermediate results! Are you sure?';
};

// Also pass window information to shiny upon window resize
window.addEventListener('resize', ev => {
  handlers.passWindowInformation();
});

// document.addEventListener('DOMContentLoaded', () => {
//   handlers.lazyLoadPackages(1);
// });
