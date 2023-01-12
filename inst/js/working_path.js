//
//
// gonna have to do this the old fashioned way.
//


const margin_tree = {top: 10, right: 10, bottom: 10, left: 10};

var width_tree;
var height_tree;
var root;
var padding = 1;
var the_anchor;


var plotDiv_tree = treepltdiv.node();
var svg_tree = treepltdiv
    .append("svg");


//  width_tree = plotDiv_tree.clientWidth - margin_tree.left - margin_tree.right;
//  height_tree = plotDiv_tree.clientHeight - margin_tree.top - margin_tree.bottom;


//  svg_tree
//    .attr("width", width_tree + margin_tree.left + margin_tree.right)
//    .attr("height", height_tree + margin_tree.top + margin_tree.bottom)
//    .append("g")
//    .attr("transform", `translate(${margin_tree.left},${margin_tree.top})`);

var data_tree, sFunGet;
var s = new XMLSerializer();
var mdpt;

Shiny.addCustomMessageHandler("data_tree",
    function(message) {
        data_tree=message.data;
        the_anchor=message.current_anchor.toString();

        redraw_tree();

})

function redraw_tree(){


  width_tree = plotDiv_tree.clientWidth - margin_tree.left - margin_tree.right;
  height_tree = plotDiv_tree.clientHeight - margin_tree.top - margin_tree.bottom;
  mdpt = height_tree/2;

  console.log(height_tree)

  svg_tree.selectAll(".remover").remove();
        svg_tree
        .attr("viewBox", [-margin_tree.left, -margin_tree.top, width_tree-margin_tree.right, height_tree-margin_tree.bottom]);
        //.attr("viewBox", "-40 -40 300 300")
        //.attr("width", width_tree + margin_tree.left + margin_tree.right)
        //.attr("height", height_tree + margin_tree.top + margin_tree.bottom)
      //.append("g")
      //  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");  // bit of margin on the left = 40

  var root = d3.stratify().id(function (d) { return d.id;}).parentId(function (d) { return d.parentId;})(data_tree);

console.log(root.height)
    // Compute the layout.
  var dx = 30;
  var dy = width_tree / (root.height + padding);
  d3.tree().nodeSize([dx, dy])(root);

  // Center the tree.
  let x0 = Infinity;
  let x1 = -x0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
  });



  // Compute the default height.
// var height = x1 - x0 + dx * 2;

  console.log(root.descendants())

 if(root.descendants().slice(1).length > 0){
  svg_tree.selectAll('path')
    .data(root.links())
      .join("path")
      .attr("fill", "none")
      .attr("stroke", "#555")
      .attr("stroke-opacity", .7)
      //.attr("stroke-linecap", strokeLinecap)
      //.attr("stroke-linejoin", strokeLinejoin)
      .attr("stroke-width", 1.5)
        .attr("d", d3.linkHorizontal()
            .x(d => d.y)
            .y(d => d.x + mdpt));
}

    svg_tree.selectAll("text")
    .data(root.descendants())
    .join("text")
      .attr("class", "remover")
      .style("font-size", "12px")
      .attr("y", function(d) {return d.x + mdpt})
      .attr("x", function(d) { return d.y;})
      .attr("dx", -6)
      .attr("dy", 20)
      .text(function(d) { return d.data.name;});

      svg_tree.selectAll("circle")
      .data(root.descendants())
      .join("circle")
      .attr("class", "remover")
      .attr("id", function (d) { return d.id; })
      .attr("cx", function(d) {
          return d.y
          //return `translate(40,40)`
      })
      .attr("cy", function(d) {
          return d.x + mdpt
          //return `translate(40,40)`
      })
        .attr("r", 7)
        .style("fill", "#69b3a2")
        .attr("stroke", function (d) {return d.id==the_anchor ? "black" : "B8B8B8" })
        .style("stroke-width", function (d) {return d.id==the_anchor ? 4 : 1 })
        .on("mouseover", handleMouseOverTree)
        .on("mouseout", handleMouseOutTree)
        .on("click", update_anchor)



}

function handleMouseOverTree(event, d) {
            // Use D3 to select element, change color back to normal
    if(d3.select(this).attr("id")!==the_anchor){
    d3.select(this)
    .style("stroke-width", 4)
    }
};

function handleMouseOutTree(event, d) {
            // Use D3 to select element, change color back to normal
    if(d3.select(this).attr("id")!==the_anchor){
    d3.select(this)
    .style("stroke-width", 1)
    }

};

function update_anchor(d) {

    const anchor_select = d3.select(this).attr("id");
    the_anchor = anchor_select;


    // Get rid of until go to shiny
        Shiny.setInputValue(
          stanch,
          the_anchor,
          {priority: "event"}
        );

        svg_tree.selectAll("circle")
          .style("stroke-width", 1)
          .attr("stroke", "#B8B8B8");
        d3.select(this)
          .style("stroke-width", 4)
          .attr("stroke", "black")
            // Use D3 to select element, change color back to normal

}



window.addEventListener("resize", redraw_tree);
