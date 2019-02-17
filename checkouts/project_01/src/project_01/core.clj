;;; Project 01
;;; simulating a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(ns project-01.core)

(defn square [x]
  (* x x))

(def gravity 9.8) ;; in m/s
(def pi 3.14159)

;; Problem 01

(defn position [a v u t]
  (+ (* (* 0.5 a) (square t)) (* v t) u))

;; Problem 02

(defn root1 [a b c]
  (let [z (- (square b) (* 4 a c))]
    (if (< z 0)
      false
      (/ (+ (- b) (Math/sqrt z)) (* 2 a)))))

(defn root2 [a b c]
  (let [z (- (square b) (* 4 a c))]
    (if (< z 0)
      false
      (/ (- (- b) (Math/sqrt z)) (* 2 a)))))

;; Problem 03

(defn time-to-impact [vertical-velocity elevation]
  (root2 (/ (- gravity) 2.0) vertical-velocity elevation))

(defn time-to-height [vertical-velocity elevation target-elevation]
  (time-to-impact vertical-velocity (- elevation target-elevation)))

;; Problem 04

(defn degree2radian [deg]
  (/ (* deg pi) 180.0))

(defn travel-distance-simple [elevation velocity angle]
  (let [rangle (degree2radian angle)
        vx (* velocity (Math/cos rangle))
        vy (* velocity (Math/sin rangle))
        impact-time (time-to-impact vy elevation)]
    (position 0 vx 0 impact-time)))

;; Problem 05

(defn find-best-angle [velocity elevation]
  (defn iter [a curr-max-distance curr-max-angle]
    (if (> a 90)
      curr-max-angle
      (let [next-angle (+ a 1)
            test-distance (travel-distance-simple elevation velocity a)]
        (if (> test-distance curr-max-distance)
          (iter next-angle test-distance a)
          (iter next-angle curr-max-distance curr-max-angle)))))
  (iter 0.0 0.0 0.0))

;; Problem 06

