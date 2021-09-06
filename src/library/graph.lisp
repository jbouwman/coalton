(cl:in-package #:coalton-user)

;;
;; Library module for graphs and graph algorithms
;;


;; The graph data representation and operations in this file are
;; adapted from
;;
;;    https://github.com/petgraph/petgraph
;;
;; which is licensed under the MIT License
;; (https://github.com/petgraph/petgraph/blob/master/LICENSE-MIT)

;; Copyright (c) 2015
;;
;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
;; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; TODO: check functinality for undirected graphs including graphviz output
;; TODO: check license and attribution in this file with @stylewarning

(coalton-toplevel

  ;;
  ;; Index types
  ;;

  (define-type (IndexPair :a)
    "A pair of indices representing the incoming and outgoing edges on a node."
    (IndexPair :a :a))

  (declare index-pair-incoming ((IndexPair :a) -> :a))
  (define (index-pair-incoming p)
    (match p
      ((IndexPair _ x) x)))

  (declare index-pair-outgoing ((IndexPair :a) -> :a))
  (define (index-pair-outgoing p)
    (match p
      ((IndexPair x _) x)))

  (define-type EdgeIndex
    (EdgeIndex Int))

  (declare edge-index-value (EdgeIndex -> Int))
  (define (edge-index-value idx)
    (match idx
      ((EdgeIndex idx)
       idx)))

  (define-instance (Into EdgeIndex Int)
    (define (into x)
      (match x
	((EdgeIndex x) x))))

  (define-instance (Eq EdgeIndex)
    (define (== a b)
      (match (Tuple a b)
	((Tuple (EdgeIndex a) (EdgeIndex b))
	 (== a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Show EdgeIndex)
    (define (show x)
      (match x
	((EdgeIndex x) (show x)))))

  (define-type NodeIndex
    (NodeIndex Int))

  (define-instance (Show NodeIndex)
    (define (show x)
      (match x
	((NodeIndex x) (show x)))))

  (declare node-index-value (NodeIndex -> Int))
  (define (node-index-value idx)
    (match idx
      ((NodeIndex idx)
       idx)))

  (define-instance (Into NodeIndex Int)
    (define (into x)
      (match x
	((NodeIndex x) x))))

  (define-instance (Eq NodeIndex)
    (define (== a b)
      (match (Tuple a b)
	((Tuple (NodeIndex a) (NodeIndex b))
	 (== a b))))
    (define (/= a b)
      (not (== a b))))

  ;;
  ;; Node and edge types
  ;;

  (define-type (Node :data)
    (Node :data (IndexPair (Optional EdgeIndex))))

  (declare node-data ((Node :data) -> :data))
  (define (node-data node)
    (match node
      ((Node x _) x)))

  (declare node-edge-pair ((Node :data) -> (IndexPair (Optional EdgeIndex))))
  (define (node-edge-pair node)
    "Gets the IndexPair of the first incoming and outgoing edge for the node."
    (match node
      ((Node _ x) x)))

  (define-type (Edge :data)
    (Edge :data (IndexPair (Optional EdgeIndex)) (IndexPair NodeIndex)))

  (declare edge-data ((Edge :data) -> :data))
  (define (edge-data edge)
    (match edge
      ((Edge data _ _)
       data)))

  (declare edge-next-pair ((Edge :data) -> (IndexPair (Optional EdgeIndex))))
  (define (edge-next-pair edge)
    (match edge
      ((Edge _ next _)
       next)))

  (declare edge-node-pair ((Edge :data) -> (IndexPair NodeIndex)))
  (define (edge-node-pair edge)
    (match edge
      ((Edge _ _ nodes)
       nodes)))

  (declare edge-from-index ((Edge :data) -> NodeIndex))
  (define (edge-from-index edge)
    (match edge
      ((Edge _ _ (IndexPair from _))
       from)))

  (declare edge-to-index ((Edge :data) -> NodeIndex))
  (define (edge-to-index edge)
    (match edge
      ((Edge _ _ (IndexPair _ to))
       to)))


  ;;
  ;; Graph type
  ;;

  (define-type GraphType
    Undirected
    Directed)

  (define-type (Graph :node-data :edge-data)
    "A graph using adjacency list representation"
    (Graph GraphType
	   (Vector (Node :node-data))
	   (Vector (Edge :edge-data))))


  (declare make-graph (Unit -> (Graph :node-data :edge-data)))
  (define (make-graph _)
    "Create a new empty undirected graph"
    (Graph Undirected
	   (make-vector Unit)
	   (make-vector Unit)))

  (declare make-digraph (Unit -> (Graph :node-data :edge-data)))
  (define (make-digraph _)
    "Create a new directed graph"
    (Graph Undirected
	   (make-vector Unit)
	   (make-vector Unit)))

  (declare graph-nodes ((Graph :node-data :edge-data) -> (Vector (Node :node-data))))
  (define (graph-nodes graph)
    "Returns the nodes in a graph"
    (match graph
      ((Graph _ nodes _) nodes)))

  (declare graph-edges ((Graph :node-data :edge-data) -> (Vector (Edge :edge-data))))
  (define (graph-edges graph)
    "Returns the edges in a graph"
    (match graph
      ((Graph _ _ edges) edges)))

  (declare graph-is-directed ((Graph :node-data :edge-data) -> Boolean))
  (define (graph-is-directed graph)
    (match graph
      ((Graph (Directed) _ _) True)
      ((Graph (Undirected) _ _) False)))


  (declare graph-node-count ((Graph :node-data :edge-data) -> Int))
  (define (graph-node-count graph)
    "Returns the number of nodes in a graph"
    (vector-length (graph-nodes graph)))

  (declare graph-edge-count ((Graph :node-data :edge-data) -> Int))
  (define (graph-edge-count graph)
    "Returns the number of edges in a graph"
    (vector-length (graph-edges graph)))


  (declare graph-lookup-node (NodeIndex -> (Graph :node-data :edge-data) -> (Optional (Node :node-data))))
  (define (graph-lookup-node idx g)
    "Lookup a node with index IDX in graph G"
    (vector-index (into idx) (graph-nodes g)))

  (declare graph-lookup-edge (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional (Edge :edge-data))))
  (define (graph-lookup-edge idx g)
    "Lookup a node with index IDX in graph G"
    (vector-index (into idx) (graph-edges g)))


  (declare graph-add-node (:node-data -> (Graph :node-data :edge-data) -> NodeIndex))
  (define (graph-add-node node-data graph)
    "Add a node with associated data to the graph, returning the index of the new node."
    (match graph
      ((Graph _ nodes _)
       (progn
	 (vector-push (Node node-data (IndexPair None None)) nodes)
	 (NodeIndex (vector-length nodes))))))

  (declare graph-remove-node (NodeIndex -> (Graph :node-data :edge-data) -> (Optional :node-data)))
  (define (graph-remove-node index graph)
    "Remove a node and all edges connecting to it from GRAPH"
    (let ((remove-edges
            (fn (idx accessor)
              (do (node_ <- (vector-index idx (graph-nodes graph)))
                  (match (accessor (node-edge-pair node_))
                    ((None)
                     None)
                    ((Some edge)
                     (progn
                       (fromSome "Internal bug" (graph-remove-edge edge graph))
                       (remove-edges idx accessor))))))))
      (do (node_ <- (vector-index (into index) (graph-nodes graph)))
          (remove-edges (into index) index-pair-incoming)
          (remove-edges (into index) index-pair-outgoing)
          (node_ <- (vector-swap-remove (into index) (graph-nodes graph)))
          (match (vector-index (into index) (graph-nodes graph))
            ((None)
             (Some (node-data node_)))
            ((Some ed)
             (let ((swap-edges (node-edge-pair ed))
                   (old-index (NodeIndex (vector-length (graph-nodes graph))))
                   (accessor index-pair-incoming)

                   (adjust-edges
                     (fn (idx dir-node-accessor dir-edge-accessor dir-update)
                       (match (graph-lookup-edge idx graph)
                         ((None)
                          None)
                         ((Some e)
                          (progn
                            (unless (== (dir-node-accessor (edge-node-pair e))
                                        old-index)
                              (error "Found edge with incorrect node index"))

                            (vector-set (into idx)
                                        (Edge (edge-data e)
                                              (edge-next-pair e)
                                              (dir-update (edge-node-pair e) index))
                                        (graph-edges graph))

                            (match (dir-edge-accessor (edge-next-pair e))
                              ((None)
                               None)
                              ((Some next-idx)
                               (adjust-edges next-idx dir-node-accessor dir-edge-accessor dir-update)))))))))
               (progn
                 (match (index-pair-incoming swap-edges)
                   ((None) None)
                   ((Some edge)
                    (adjust-edges edge
                                  index-pair-incoming
                                  index-pair-incoming
                                  (fn (pair val)
                                    (IndexPair (index-pair-outgoing pair)
                                               val)))))

                 (match (index-pair-outgoing swap-edges)
                   ((None) None)
                   ((Some edge)
                    (adjust-edges edge
                                  index-pair-outgoing
                                  index-pair-outgoing
                                  (fn (pair val)
                                    (IndexPair val
                                               (index-pair-incoming pair))))))

                 (Some (node-data node_)))))))))


  (declare graph-add-edge (:edge-data -> NodeIndex -> NodeIndex -> (Graph :node-data :edge-data) -> EdgeIndex))
  (define (graph-add-edge edge-data from to graph)
    "Add an edge with associated data from node FROM to node TO in the graph."
    ;; Create a new edge index with the current length of the vector
    (let ((edge-idx (EdgeIndex (graph-edge-count graph))))
      (progn
        ;; Check that both indices are valid
        (let node-count = (graph-node-count graph))
        (unless (and (< (node-index-value from) node-count)
                     (< (node-index-value to)   node-count))
          (error "Invalid node index when adding graph edge"))

        (if (== from to)
            ;; If FROM == TO then we are creating a self-cycle
	    (progn
              ;; Pull the exiting node out of the graph
	      (let node_ = (fromSome "unreachable" (graph-lookup-node from graph)))
              ;; Construct the edge, pointing to the previous heads of the edge lists
	      (let edge_ = (Edge edge-data
                                 (node-edge-pair node_)
                                 (IndexPair from to)))
              ;; Construct a new node with the new edge as the head of the incoming/outgoing edge lists
	      (let node__ = (Node (node-data node_)
                                  (IndexPair (Some edge-idx) (Some edge-idx))))

              ;; Finally, add the edge and replace the node
	      (vector-push edge_ (graph-edges graph))
	      (vector-set (into from) node__ (graph-nodes graph))
              ;; And return the index of the new edge
	      edge-idx)

            ;; Otherwise, create non-cycle (using two nodes)
            (progn
              ;; Pull the exiting nodes out of the graph
	      (let from-node = (fromSome "unreachable" (graph-lookup-node from graph)))
              (let to-node   = (fromSome "unreachable" (graph-lookup-node to graph)))
              ;; Construct the edge, pointing to the previous heads of the edge lists
	      (let edge_ = (Edge edge-data
                                 (IndexPair (index-pair-outgoing (node-edge-pair from-node))
                                            (index-pair-incoming (node-edge-pair to-node)))
                                 (IndexPair from to)))
              ;; Construct new nodes with the new edge as the head of the incoming/outgoing edge lists
	      (let from-node_ = (Node (node-data from-node)
                                      (IndexPair (Some edge-idx)
                                                 (index-pair-incoming (node-edge-pair from-node)))))
              (let to-node_   = (Node (node-data to-node)
                                      (IndexPair (index-pair-outgoing (node-edge-pair to-node))
                                                 (Some edge-idx))))

              ;; Finally, add the edge and replace the nodes
	      (vector-push edge_ (graph-edges graph))
	      (vector-set (into from) from-node_ (graph-nodes graph))
              (vector-set (into to) to-node_ (graph-nodes graph))
              ;; And return the index of the new edge
	      edge-idx)))))

  (declare graph-remove-edge (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional :edge-data)))
  (define (graph-remove-edge index graph)
    "Remove an edge from GRAPH"
    (do (edge <- (vector-index (into index) (graph-edges graph)))
        (let _ = (change-edge-links (edge-node-pair edge) index (edge-next-pair edge) graph))
        (remove-edge-adjust-indices index graph)))

  ;; Helpers for above function

  (declare change-edge-links ((IndexPair NodeIndex) -> EdgeIndex -> (IndexPair (Optional EdgeIndex)) -> (Graph :node-data :edge-data) -> Unit))
  (define (change-edge-links edge-node index edge-next graph)
    (let ((change-edge-links-direction
            ;; NOTE: We need two different accessors because we are
            ;; not able to polymorphise on arguments (when pulling
            ;; parts of IndexPairs out)
            (fn (dir-node-accessor dir-edge-accessor dir-update)
              (let ((node_ (fromSome "Edge endpoint not found"
                                     (graph-lookup-node (dir-node-accessor edge-node) graph))))
                (let ((fst (dir-edge-accessor (node-edge-pair node_))))
                  (if (if (isSome fst)
                          (== index (fromSome "" fst))
                          False)
                      (const Unit
                             (vector-set (into (dir-node-accessor edge-node))
                                         (Node (node-data node_)
                                               (dir-update (node-edge-pair node_)
                                                           (dir-edge-accessor edge-next)))
                                         (graph-nodes graph)))
                      (let ((replace-edge
                              (fn (idx)
                                (match (graph-lookup-edge idx graph)
                                  ((None)
                                   Unit)
                                  ((Some e)
                                   (let ((next-pair (edge-next-pair e)))
                                     (match (dir-edge-accessor next-pair)
                                       ((None)
                                        Unit)
                                       ((Some next-idx)
                                        (if (== next-idx index)
                                            (const Unit
                                                   (vector-set (into idx)
                                                               (Edge (edge-data e)
                                                                     (dir-update next-pair
                                                                                 (dir-edge-accessor edge-next))
                                                                     (edge-node-pair e))
                                                               (graph-edges graph)))
                                            (replace-edge next-idx))))))))))
                        (replace-edge index))))))))

      (progn
        ;; First replace the incoming side
        (change-edge-links-direction
         index-pair-incoming
         index-pair-incoming
         (fn (pair val)
           (IndexPair (index-pair-outgoing pair)
                      val)))

        ;; Then replace the outgoing
        (change-edge-links-direction
         index-pair-outgoing
         index-pair-outgoing
         (fn (pair val)
           (IndexPair val
                      (index-pair-incoming pair)))))))

  (declare remove-edge-adjust-indices (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional :edge-data)))
  (define (remove-edge-adjust-indices index graph)
    (do (edge_ <- (vector-swap-remove (into index) (graph-edges graph)))
        (match (vector-index (into index) (graph-edges graph))
          ((None)
           (Some (edge-data edge_)))
          ((Some ed)
           (let ((swap (edge-node-pair ed))
                 (swapped-edge (EdgeIndex (vector-length (graph-edges graph)))))
             (progn
               (change-edge-links swap swapped-edge (IndexPair (Some index) (Some index)) graph)
               (Some (edge-data edge_))))))))

  (declare graph-viz (Show :node-data => ((Graph :node-data :edge-data) -> String)))
  (define (graph-viz graph_)
    (progn
      (let elements = (make-vector Unit))
      (if (graph-is-directed graph_)
	  (vector-push "digraph {" elements)
	  (vector-push "graph {" elements))
      (vector-foreach-index
       (fn (i node)
	(vector-push
	  (msum
	  (make-list
	    (show i)
	    " [label=\""
	    (show (node-data node))
	    "\"]"))
	  elements))
       (graph-nodes graph_))
      (vector-foreach
       (fn (edge)
	(vector-push
	  (msum
	  (make-list 
	    (show (edge-from-index edge))
	    (if (graph-is-directed graph_)
		" -> "
		" -- ")
	    (show (edge-to-index edge))))
	  elements))
       (graph-edges graph_))
      (vector-push "}" elements)
      (msum (intersperse (show #\NEWLINE) (vector-to-list elements))))))


