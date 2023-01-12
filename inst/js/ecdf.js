
const margin = {top: 10, right: 10, bottom: 10, left: 10};

var width_ecdf;
var height_ecdf;

var reverse = 0;

let xScale_ecdf = {};
let yScale_ecdf = {};

var plotDiv_ecdf = filterpltdiv.node();
var svg_ecdf = filterpltdiv
    .append("svg");

var prob_cut = .05;

Shiny.addCustomMessageHandler("set_prob_cut",
    function(message) {
        prob_cut = message;

})

//var data_ecdf = {};

var data_ecdf

Shiny.addCustomMessageHandler("data_ecdf",
    function(message) {
        data_ecdf=message;
        redraw_ecdf();
        Shiny.setInputValue(
          "has_ecdf",
          true,
          {priority: "event"}
        );

})

Shiny.addCustomMessageHandler("clear_ecdf",
    function(message) {
        svg_ecdf.selectAll("*").remove();
    Shiny.setInputValue(
          "has_ecdf",
          false,
          {priority: "event"}
        );
})

Shiny.addCustomMessageHandler("invert_selection",
  function(message){
    reverse = 1-reverse;
    draw_rect_box(prob_cut);
})

Shiny.addCustomMessageHandler("clear_invert",
  function(message){
   reverse = 0;
   prob_cut=.05;
  })

function redraw_ecdf() {
  console.log('heredraw')
  width_ecdf = plotDiv_ecdf.clientWidth - margin.left - margin.right;
  height_ecdf = plotDiv_ecdf.clientHeight - margin.top - margin.bottom;

  svg_ecdf.selectAll("*").remove()

  svg_ecdf
    .attr("width", width_ecdf + margin.left + margin.right)
    .attr("height", height_ecdf + margin.top + margin.bottom)
    .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);



  set_scales(data_ecdf);


  draw_ecdf(data_ecdf);

  draw_horizontal_line(prob_cut);

  draw_rect_box(prob_cut);

  svg_ecdf
         .on("click", update_horizontal_line);

}

function set_scales(dat){

  xScale_ecdf = d3.scaleLinear()
    .domain([d3.min(dat, function(d) { return +d.x; }), d3.max(dat, function(d) { return +d.x; })])
    .range([ 0, width_ecdf ]);

  svg_ecdf.append("g")
      .attr("transform", `translate(0, ${height_ecdf})`)
      .call(d3.axisBottom(xScale_ecdf));


  // Add Y axis  -- need to define but don't need to add
  yScale_ecdf = d3.scaleLinear()
    .domain([0, 1])
    .range([ height_ecdf, 0]);

  svg_ecdf.append("g")
      .call(d3.axisLeft(yScale_ecdf));

}

function draw_ecdf(dat){
svg_ecdf.append("path")
      .datum(dat)
      .attr("fill", "none")
      .attr("stroke", "steelblue")
      .attr("stroke-width", 1.5)
      .attr("d", d3.line()
        .x(function(d) { return xScale_ecdf(d.x) })
        .y(function(d) { return yScale_ecdf(d.y) }));
}



function draw_horizontal_line(yPos)
  {
    svg_ecdf
       .append("line")
       .attr("id", "theline")
       .attr("x1", 0)
       .attr("x2", width_ecdf)
       .attr("y1", yScale_ecdf(yPos))
       .attr("y2", yScale_ecdf(yPos))
       .attr("stroke", "#2ecc71")
       .attr("stroke-width", "1px");
  };

  function update_horizontal_line (e) {
    var prob_cut_coord = d3.pointer(e, this);

    prob_cut = yScale_ecdf.invert(prob_cut_coord[1]);

    if(prob_cut > 0 & prob_cut < 1){
    svg_ecdf.selectAll("line").remove();
    draw_horizontal_line(prob_cut);
    draw_rect_box(prob_cut);
    Shiny.setInputValue(
          lclick,
          prob_cut,
          {priority: "event"}
        );

}

  };


function draw_rect_box(yPos)
  {
    svg_ecdf.selectAll("rect").remove();
    svg_ecdf
       .append("rect")
       .attr("id", "therect")
       .attr("x", 0)
       .attr("width", width_ecdf)
       .attr("fill", "#808080")
       .attr("opacity", .2)
       .attr("stroke-opacity", "0%");

    if(reverse===0){
      svg_ecdf.select('#therect')
        .attr("y", yScale_ecdf(yPos))
        .attr("height", height_ecdf-yScale_ecdf(yPos));
    } else {
      svg_ecdf.select('#therect')
        .attr("y", 0)
        .attr("height", yScale_ecdf(yPos));
    }
  };



window.addEventListener("resize", redraw_ecdf);
        //end code past here
