

var margin = {top: 30, right: 30, bottom: 70, left: 60},
    width = 200 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;
    
var svg = d3.select("#wrapper_bar2")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

svg.selectAll("g")
  .data(data)
  .enter()
  .append("text")
  .text(function(d) {
    return d.textOriginal;
  });