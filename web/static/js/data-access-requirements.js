function show_additional_duo () {
    jQuery("#additional-limitations").show();
}

function hide_additional_duo () {
    jQuery("#additional-limitations").hide();
}

function with_autocomplete(callback) {
  jQuery.ajax("/form/data-access-requirements/api/all-studies", {
    headers: { "Accept": "application/json" },
    method: "GET",
    success: function (data) { callback(data); }});
}

// function with_dataset(studyId, callback) {
//   jQuery.ajax("/form/data-access-requirements/api/datasets-by-study-id", {
//     headers: { "Accept": "application/json", "Content-Type": "application/json" },
//     method: "POST",
//     data: JSON.stringify({ "study-id": studyId }),
//     success: function (data) { callback(data.datasets); }});
// }

function proceed(section) {
  jQuery("#accordion").accordion("option", "active", section);
}

function toggle_consent_box() {
  var state = document.getElementById("consent-box").checked;
  if (state) {
    jQuery("#consent-form-location-id").show();
  } else {
    jQuery("#consent-form-location-id").hide();
  }
}

function submit_form(form) {
  jQuery("#dar-form").submit();
}

function ac_dataset(studyId) {
  with_dataset(studyId, function (dataset) {
    console.log("Dataset: "+ dataset);
    if (dataset.length == 0)
      return;
    if (dataset.length == 1) {
      jQuery("#dataset-id").val(dataset);
    }
    else
      jQuery("#dataset-id").autocomplete({ source: dataset });
  });
}

jQuery(document).ready(function(){
  jQuery("#accordion").accordion({ animate: 0 });
    if (! jQuery("#consent-box").is(":checked")) {
      jQuery("#consent-form-location-id").hide();
    }

  if (jQuery("#dul-radio").is(":checked") &&
      (! jQuery("#DUO_0000004").is(":checked"))) {
    hide_additional_duo ();
  }

  // Autocompletion handlers
  // with_autocomplete(function (data) {
  //   jQuery("#dataset-id") //.autocomplete({ source: data });
  //     .autocomplete("instance")._renderItem = function(ul, item) {
  //       return jQuery("<li>")
  //                .append("<div>" + item.datasetId
  //                        + "<br>" + item.datasetTitle
  //                        + "</div>")
  //                .appendTo(ul);
  //     }
  // });
});
