

var margin = {top: 30, right: 30, bottom: 70, left: 60},
    width = 2000 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;
    
var svg = d3.select("#wrapper_bar")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// X axis
var x = d3.scaleBand()
  .range([ 0, width ])
  .domain(data.map(function(d) { return d.word; }))
  .padding(0.2);
svg.append("g")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x))
  .selectAll("text")
    .attr("transform", "translate(-10,0)rotate(-45)")
    .style("text-anchor", "end");

// Add Y axis
var y = d3.scaleLinear()
  .domain([0, 100])
  .range([ height, 0]);
svg.append("g")
  .call(d3.axisLeft(y));

// Bars
svg.selectAll("mybar")
  .data(data)
  .enter()
  .append("rect")
    .attr("x", function(d) { return x(d.word); })
    .attr("y", function(d) { return y(d.total); })
    .attr("width", x.bandwidth())
    .attr("height", function(d) { return height - y(d.total); })
    .attr("fill", "#69b3a2");


