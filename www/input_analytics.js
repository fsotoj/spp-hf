/******************************************************
 * UNIVERSAL SHINY INPUT TRACKING
 * Sends event: "input_change"
 ******************************************************/

/******************************************************
 * RE-DEFINED SAFE GTAG (IFRAME BRIDGE VERSION)
 ******************************************************/
function safeGtag() {
    const args = Array.from(arguments);
    
    // Check if we are inside an iframe
    if (window.self !== window.top) {
        // Send arguments to the parent window
        window.parent.postMessage({
            type: 'SHINY_GA_EVENT',
            payload: args
        }, '*'); 
    } else {
        // Fallback: If you open the HF link directly, try local gtag
        if (typeof gtag === "function") {
            gtag.apply(null, args);
        } else {
            console.log("Not in iframe & no local gtag. Event:", args);
        }
    }
}

// This captures ALL input changes:
$(document).on("shiny:inputchanged", function (e) {

  // Ignore internal Shiny stuff
  if (!e.name || e.name.startsWith(".clientdata")) return;
  if (e.name.includes("shinydashboard.sidebar")) return;

  safeGtag("event", "input_change", {
    input_id: e.name,
    value: typeof e.value === "object"
      ? JSON.stringify(e.value)
      : String(e.value)
  });

});
