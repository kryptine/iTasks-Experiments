module Graphlet

import iTasks
import iTasks.API.Extensions.Graphlet.Graphlet
import iTasks.API.Extensions.Graphlet.GraphvizRenderer
import Data.Graph

//testGraphlet = viewInformation "Graphlet with Graphviz-like rendering" []
//                 (graphletEditlet graphvizRenderer {graph = mkTestGraph}) <<@ FullScreen
testGraphlet = mkTask (graphletTasklet graphvizRenderer {graph = mkTestGraph})
  where
  mkTestGraph
    // Create a new, empty graph
    # g         = emptyGraph

    // Add all nodes
    // Addings a node returns a node identfier, which can be used to connect
    // edges to the node.
    // Text inside nodes is represented by a list of strings. Each string
    // corresponds to one line.
    # (ni1, g)  = addNode (GSBox (Just ["Motor Learning"])) g
    # (ni2, g)  = addNode (GSBox (Just ["Effort"])) g
    # (ni3, g)  = addNode (GSBox (Just ["Intention & Motivation"])) g
    # (ni4, g)  = addNode (GSBox (Just ["Set Formation", "(Practice Instructions)"])) g
    # (ni5, g)  = addNode (GSDiamond Nothing) g
    # (ni6, g)  = addNode (GSBox (Just ["Focused Attention"])) g
    # (ni7, g)  = addNode (GSBox (Just ["Working Memory"])) g
    # (ni8, g)  = addNode (GSPlainText ["Orienting"]) g
    # (ni9, g)  = addNode (GSPlainText ["Engagement"]) g
    # (ni10, g) = addNode (GSEllipse (Just ["Intended Object", "(Breath)"])) g
    # (ni11, g) = addNode (GSEllipse (Just ["Unintended Object", "(Sensory or Mental Event)"])) g
    # (ni12, g) = addNode (GSBox (Just ["Affective Response + - 0"])) g
    # (ni13, g) = addNode (GSBox (Just ["Episodic & Procedural", "Memory"])) g
    # (ni14, g) = addNode (GSBox (Just ["Executive Monitoring", "(meta-awareness)"])) g
    # (ni15, g) = addNode (GSBox (Just ["Decentering"])) g
    # (ni16, g) = addNode (GSBox (Just ["Response Inhibition"])) g
    # (ni17, g) = addNode (GSBox (Just ["Emotion Regulation"])) g
    # (ni18, g) = addNode (GSPlainText ["Disengagement"]) g

    // Add all edges
    // An edge is a tuple of node identifiers.
    // Text is handled the same way as with nodes.
    # g         = addEdge (Just ["+"]) (ni1, ni2) g
    # g         = addEdge (Just ["-"]) (ni2, ni3) g
    # g         = addEdge Nothing (ni3, ni1) g
    # g         = addEdge Nothing (ni2, ni3) g
    # g         = addEdge Nothing (ni3, ni4) g
    # g         = addEdge Nothing (ni4, ni5) g
    # g         = addEdge Nothing (ni5, ni6) g
    # g         = addEdge Nothing (ni5, ni7) g
    # g         = addEdge Nothing (ni5, ni8) g
    # g         = addEdge Nothing (ni8, ni9) g
    # g         = addEdge Nothing (ni9, ni10) g
    # g         = addEdge Nothing (ni10, ni5) g
    # g         = addEdge (Just ["Distraction"]) (ni10, ni11) g
    # g         = addEdge Nothing (ni11, ni12) g
    # g         = addEdge Nothing (ni12, ni13) g
    # g         = addEdge Nothing (ni13, ni11) g
    # g         = addEdge Nothing (ni13, ni15) g
    # g         = addEdge (Just ["+ Clarity"]) (ni14, ni10) g
    # g         = addEdge Nothing (ni14, ni18) g
    # g         = addEdge Nothing (ni14, ni15) g
    # g         = addEdge Nothing (ni15, ni11) g
    # g         = addEdge Nothing (ni15, ni17) g
    # g         = addEdge Nothing (ni16, ni15) g
    # g         = addEdge Nothing (ni16, ni17) g
    # g         = addEdge Nothing (ni17, ni18) g
    # g         = addEdge (Just ["+ Equanimity"]) (ni17, ni16) g
    # g         = addEdge Nothing (ni18, ni15) g

    // Return the graph
    = g

Start :: *World -> *World
Start world = startEngine testGraphlet world


