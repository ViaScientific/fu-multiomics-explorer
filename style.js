[
  {"selector":"node", "css": {
    "content": "data(label)",
    "text-valign":"center",
    "text-halign":"center",
    "border-width": "1px"
  }},
  
  {"selector": "node[log2FC<=0]", "css": {
  	"background-color": "mapData(log2FC, -.5, 0, #4682B4, #F5F5F5)"
   }},

  {"selector": "node[log2FC>0]", "css": {
		"background-color": "mapData(log2FC, 0, 1, #F5F5F5, #b22222)"
  }},
  
  {"selector": "node:selected", "css": {
    "overlay-opacity": 0.3,
    "overlay-color": "gray"
  }},
  
  {"selector": "edge[log2FC<=0]", "css": {
    "content": "data(Symbol)",
    "line-color": "mapData(log2FC, -3, 0, #4682B4, #F5F5F5)",
    "line-width": "1px"
  }},
  
  {"selector": "edge[log2FC>0]", "css": {
    "content": "data(Symbol)",
    "line-color": "mapData(log2FC, 0, 3, #F5F5F5, #b22222)",
    "line-width": "1px"
  }}
]