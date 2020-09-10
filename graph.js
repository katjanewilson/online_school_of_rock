
// !preview r2d3 data = read.csv("anger.csv"), d3_version = 4
//
// r2d3: https://rstudio.github.io/r2d3
//

d3.csv("anger.csv"), (function(data){
   console.log(data);
});

var w = 900;
var h = 500;
var barPadding = 2;

var svg = d3.select("#chart-area1").append("svg")
   .attr("width", w)
   .attr("height", h);
   
svg.selectAll("rect")
   .data(data)
   .enter()
   .append("rect")
   .attr("x", function(d,i){
     return i * (w/data.length);
   })
   .attr("y", function(d) {
     return h-(d.total *10);
   })
   .attr("width", w / data.length - barPadding)
   .attr("height", function(d) {
     return d.total*10;
   })
   .attr("fill", "teal");

svg.selectAll("text")
  .data(data)
  .enter()
  .append("text")
  .text(function(d) {
    return d.word;
})
  .attr("x", function(d,i) {
    return i * (w/data.length);
  })
  .attr("y", function(d) {
    return h-(d.total);
  })
  .attr("font-family", "sans-serif")
  .attr("font-size", "11px")
  .attr("fill", "black")
  .attr("text-anchor", "middle");
  
   