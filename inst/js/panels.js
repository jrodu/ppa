//The tooltip divs have been specified here with the particular namespace in mind.  Not great, but how it is for now.
var m = [0, 0, 0, 0];
        //w = 960 - m[1] - m[3],
        //h = 600 - m[0] - m[2];


var w=700;
var h=300;

var width;
var height;


let current_selection = [];


let sumstat = {};
let xScale = {};
let yScale = {};



//var plotDiv = document.getElementById("d3_output");
//var plotDiv = stickpltdiv.node();
//var svg = d3.select("#d3_output")
//var svg = stickpltdiv
//   .append("svg")
    //.attr("width", w)
    //.attr("height", h);



var data = {};
var data_new_centers = {};
var dataplot = [];
let extractColumn = (arr, column) => arr.map(x=>x[column]);
var plotflag;

Shiny.addCustomMessageHandler("plottypeflag",
    function(message) {
        plotflag = message;
        console.log('here')
})

Shiny.addCustomMessageHandler("data",
    function(message) {
        data=message;
        //console.log(data)
        //set_groups(data);
        //console.log(data.lines)
        // START HEREEEEEEEEEEE!!!!
        get_dataplot(data.lines, data.centers)

        //console.log(dataplot)
        //set_scales_and_groups(data);

        data.points = {};

        redraw();
})

Shiny.addCustomMessageHandler("data_new_centers",
    function(message) {
        data_new_centers=message;

        data.centers = data_new_centers.centers;

        panels_in = extractColumn(data_new_centers.centers_use, "panel_string");

        var dlinefeed = data.lines.filter(dat => panels_in.includes(dat.panel_string));

        get_dataplot(dlinefeed, data.centers);

        data.points = data_new_centers.points;

        redraw();
})


function get_dataplot(dlines, dcenters){
  dataplot=[];
          for (var i=0; i<dlines.length; i++) {
            for (var j=0; j<dcenters.length; j++) {
              if(dlines[i].panel_string === dcenters[j].panel_string)
                dataplot.push({
                dotx: dlines[i].dotx*dcenters[j].width + dcenters[j].dotcx,
                doty: dlines[i].doty*dcenters[j].height + dcenters[j].dotcy,
                panel_string: dlines[i].panel_string,
                pointid: dlines[i].pointid + "_" + dlines[i].panel_string
              });
            }
        }
}


function set_groups(dat){
    sumstat = d3.group(dat, d => d.panel_string);
}

function set_scales_and_groups(dat){
  xScale = d3.scaleLinear()
    .domain([0, d3.max(dat.centers, function(d) { return +d.dotcx + d.width; })])
    .range([ 0, width ]);


  // Add Y axis  -- need to define but don't need to add
  yScale = d3.scaleLinear()
    .domain([0, d3.max(dat.centers, function(d) { return +d.dotcy + d.height; })])
    .range([ height, 0]);

}


function update_scatter(dat){
  var lines = svg.selectAll(".datapoints")
    .data(dat, function(d){ return d.pointid; })
    .join(
      enter => enter.append("circle")
          .attr("id", function(d) { return d.pointid; })
          .attr("class", "datapoints")
          .style("fill", "#000000")
          //.attr("stroke", "#000000")//function(d){ return color(d[0]) })
          //.attr("stroke-width", .9)
          .attr("cx", function(d) { return xScale(+d.dotx); })
          .attr("cy", function(d) { return yScale(+d.doty); })
          .attr("r", 1.5),//,
        update => update
          .attr("cx", function(d) { return xScale(+d.dotx); })
          .attr("cy", function(d) { return yScale(+d.doty); })
          .attr("r", 1.5),
        exit => exit
          .remove()
          );
  }

function update_lines(dat){
  var lines = svg.selectAll("path")
      .data(dat, function(d){ return d.panel_string; })
      .join(
      enter => enter.append("path")
          .attr("id", function(d) { return d.panel_string})
          .attr("fill", "none")
          .attr("stroke", "#000000")//function(d){ return color(d[0]) })
          .attr("stroke-width", .9)
          .attr("d", function(d){
            return d3.line()
              .x(function(d) { return xScale(+d.dotx); })
             .y(function(d) { return yScale(+d.doty); })
              (d[1])
          }),
        update => update
          .attr("d", function(d){
            return d3.line()
              .x(function(d) { return xScale(+d.dotx); })
              .y(function(d) { return yScale(+d.doty); })
              (d[1])
          }),
        exit => exit
          .remove()
          );
  }





function update_rect(dat) {
  var rect = svg.selectAll("rect")
      .data(dat.centers, function(d){ return d.panel_string; })
      //.enter()
      //.append("rect")
      .join(
      enter => enter.append("rect")
          .attr("id", function(d) { return d.panel_string; })
          .attr("x", function(d) { return xScale(+d.dotcx); })
          .attr("y", function(d) { return yScale(+d.dotcy+d.height); })
          .attr("width", function(d) {return xScale(+d.width); } )
          .attr("selected", function() {return current_selection.includes(d3.select(this).attr("id")) ? 1: 0; })
      //.attr("height", function(d) {return y(+d.height); } )
          .attr("height", yScale(1) ) // need to figure out a better way to handle a "height" scale... can't be just y...
      //.attr("fill", function(d) { return myColor(+d.centers.alpha);} )
          .attr("fill", "#808080" )
          .attr("fill-opacity", function() { return (d3.select(this).attr("selected")>0) ? .5 : 0; })
          .attr("stroke-width", function(d) { return d.stroke_width; })
          .attr("stroke", "#000000")
          .attr("stroke-opacity", 1)
          .on("mouseover", handleMouseOver)
          .on("mouseout", handleMouseOut)
          .on("click", update_selection),
        update => update
          .attr("x", function(d) { return xScale(+d.dotcx); })
          .attr("y", function(d) { return yScale(+d.dotcy+d.height); })
          .attr("width", function(d) {return xScale(+d.width); } )
          .attr("height", yScale(1) )
          .attr("selected", function() {return current_selection.includes(d3.select(this).attr("id")) ? 1: 0; })
          .attr("fill-opacity", function() { return (d3.select(this).attr("selected")>0) ? .5 : 0; }),
        exit => exit
        .remove()
        );

        rect.raise();
}

function update_points(dat) {
  //console.log('herehere')
  //console.log(dat.points)
  var circle = svg.selectAll(".reduced")
      .data(dat.points, function(d){ return d.panel_string; })
      //.enter()
      //.append("rect")
      .join(
      enter => enter.append("circle")
          .attr("class", "reduced")
          .attr("id", function(d) { return d.panel_string; })
          .attr("cx", function(d) { return xScale(+d.dotcx); }) //+ d.width/2
          .attr("cy", function(d) { return yScale(+d.dotcy); }) // + d.yScale
          .attr("r", 1.5 )
          .on("mouseover", handleMouseOverPoint)
          .on("mouseout", handleMouseOutPoint)
          //.attr("selected", function() {return current_selection.includes(d3.select(this).attr("id")) ? 1: 0.8; })
      //.attr("height", function(d) {return y(+d.height); } )
          //.attr("height", yScale(1) ) // need to figure out a better way to handle a "height" scale... can't be just y...
      //.attr("fill", function(d) { return myColor(+d.centers.alpha);} )
          .attr("fill", "#808080" ),
          //.attr("opacity", function() { return (d3.select(this).attr("selected")>0) ? 1 : 0.8; })
          //.on("mouseover", handleMouseOver)
          //.on("mouseout", handleMouseOut)
          //.on("click", update_selection),
        update => update
          .attr("cx", function(d) { return xScale(+d.dotcx+d.width/2); })
          .attr("cy", function(d) { return yScale(+d.dotcy-d.height/2); })
          .attr("r", 1.5 ),
          //.attr("height", yScale(1) )
          //.attr("selected", function() {return current_selection.includes(d3.select(this).attr("id")) ? 1: .8; })
          //.attr("opacity", function() { return (d3.select(this).attr("selected")>0) ? .5 : .8; }),
        exit => exit
        .remove()
        );

}


function redraw(){
//var plotDiv = stickpltdiv.node();
//var svg = d3.select("#d3_output")
//d3.select("svg").remove()
//var svg = stickpltdiv
//    .append("svg")
    //.attr("width", w)
    //.attr("height", h);
  width = plotDiv.clientWidth;
  //height = plotDiv.clientHeight;
  height = plotDiv.clientHeight;

  svg
  //.attr("preserveAspectRatio", "xMinYMin meet")
  //.attr("viewBox", "0 0 width height")
    .attr("width", width)
    .attr("height", height)
  set_scales_and_groups(data);
  if(plotflag==="line"){
    set_groups(dataplot);
  }
        if((data.points !== undefined) && (data.points.length !== undefined)){

          data.centers = data_new_centers.centers_use;
        }

 // console.log(sumstat)
 if(plotflag==="line"){
   update_scatter({});
  update_lines(sumstat);

 } else {
  update_lines({});
  update_scatter(dataplot);
}
  update_rect(data);


  update_points(data);

}

//window.addEventListener("resize", redraw);

function handleMouseOver(e, d) {  // Add interactivity

            // Use D3 to select element, change color and size
     d3.select(this)
    .attr("fill-opacity", .7);

//Get this bar's x/y values, then augment for the tooltip
var xPosition = parseFloat(d3.select(this).attr("x"));
var yPosition = parseFloat(d3.select(this).attr("y")) - 30;
//var score = function(d) { return d.score; }

//Update the tooltip position and value
if (d.use_score===1){
//d3.select("#tooltip")
toolt
  .style("left", xPosition + "px")
  .style("top", yPosition + parseFloat(d3.select(this).attr("height")) + "px")
  .select("#mainpanel-value")
  .text("score: " + d.score);
} else if (d.use_pos===1) {
  //d3.select("#tooltip")
  toolt
  .style("left", xPosition + "px")
  .style("top", yPosition + "px")
  .select("#mainpanel-value")
  .text("x1: " + d.scorex + "\r\n x2: " + d.scorey);
}

//Show the tooltip
if(d.use_score === 1 || d.use_pos===1){
//d3.select("#tooltip")
toolt.classed("hidden", false);
}

}

function handleMouseOut(e, d) {
            // Use D3 to select element, change color back to normal
    d3.select(this).attr(
              "fill-opacity", function() {return (d3.select(this).attr("selected")>0) ? .5: 0; }
        );

    //Hide the tooltip
    //d3.select("#tooltip")
    toolt.classed("hidden", true);
}


function handleMouseOverPoint(e, d) {  // Add interactivity

            // Use D3 to select element, change color and size
    // d3.select(this)
    //.attr("opacity", .7);
//console.log('herepoint')
//Get this bar's x/y values, then augment for the tooltip
var xPosition = parseFloat(d3.select(this).attr("cx"));
var yPosition = parseFloat(d3.select(this).attr("cy")) - 30;
//var score = function(d) { return d.score; }

//Update the tooltip position and value
if (d.use_score===1){
//d3.select("#tooltip")
toolt
  .style("left", xPosition + "px")
  .style("top", yPosition + parseFloat(d3.select(this).attr("height")) + "px")
  .select("#mainpanel-value")
  .text("score: " + d.score);
} else if (d.use_pos===1) {
  //d3.select("#tooltip")
  toolt
  .style("left", xPosition + "px")
  .style("top", yPosition + "px")
  .select("#mainpanel-value")
  .text("x1: " + d.scorex + "\r\n x2: " + d.scorey);
}

//Show the tooltip
if(d.use_score === 1 || d.use_pos===1){
//d3.select("#tooltip")
toolt.classed("hidden", false);
}

}

function handleMouseOutPoint(e, d) {
            // Use D3 to select element, change color back to normal
    //d3.select(this).attr(
    //          "opacity", function() {return (d3.select(this).attr("selected")>0) ? .5: 0; }
    //    );

    //Hide the tooltip
    //d3.select("#tooltip")
    toolt.classed("hidden", true);
}



function update_selection(d) {

    const panel_select = d3.select(this).attr("id");

        //check if already selected
    const currently_selected = current_selection.includes(panel_select);


      if(currently_selected){
          current_selection = current_selection.filter(d => d!=panel_select);
        } else {
          current_selection = [...current_selection, panel_select];
        }


    // Get rid of until go to shiny
        Shiny.setInputValue(
          pclick,
          current_selection,
          {priority: "event"}
        );

        d3.select(this)
        .attr("selected", function() {return !currently_selected ? 1 : 0; })
        .attr('fill-opacity', function() {return (d3.select(this).attr("selected")>0) ? .5: 0; })
            // Use D3 to select element, change color back to normal

}



Shiny.addCustomMessageHandler("panelPlot_set", set_current_selection );

function set_current_selection(message){

  if (typeof message === 'string') {
          current_selection = [message];
        } else {// if (Array.isArray(message)) {
          current_selection = message.sel;
          if(message.centers !== undefined){
          data.centers = message.centers;

          }


        }

        update_rect(data);

      Shiny.setInputValue(
          pclick,
          current_selection,
          {priority: "event"}
        );
}

window.addEventListener("resize", redraw);
        //end code past here

