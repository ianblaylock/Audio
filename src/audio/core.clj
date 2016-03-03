(ns audio.core
  (:gen-class))
(require '[clojure.pprint :refer (cl-format)])
;;;;;required java libs;;;;;;
(import '(java.nio ByteBuffer ShortBuffer))
(import '(javax.sound.sampled DataLine AudioSystem LineEvent TargetDataLine LineListener AudioFormat))
(import '(java.util LinkedList))
(import '(java.nio ByteBuffer ByteOrder))
(import '(java.nio.file Files))
(import '(java.io File))
;;;;;utility-functions;;;;;;
(defn short-to-bytes
  "big endian"
  [s]
  [(bit-and s 0xff)
   (bit-and (bit-shift-right s 8) 0xff)])

;;;;;audio configuration basics;;;;;;;
(def sample-rate 44100)
(def sample-size 16)
(def channels 1)
(def signed true)
(def big-endian-audio-sampling true)
(def audio-format
  (AudioFormat. sample-rate sample-size channels signed big-endian-audio-sampling))
(def mixer-info (AudioSystem/getMixerInfo))
(def system-mixer-info (second mixer-info))
(def system-mixer (AudioSystem/getMixer system-mixer-info))
(def system-mixer-line-info (.getSourceLineInfo system-mixer))

;;;;;;code pertaining to my own mixer;;;;;;;;;

(def mixer-lines (atom {})) ;needs to be a bunch of linked lists...as that's the structure I'm using to queue up things!

(def dead-mixer-line-purge-time 30) ;in seconds, flush out the old queues with this return period

;(def dead-mixer-line-killer 
  ;;periodically removes keys associated with inactive mixer lines
  ;;since I plan on only writing to them when i'm generating a "note" and then abandoning them, this is necessary to keep the size of the mixer-lines down.
;  (future
;    (loop []
;      (fn []
;        (Thread/sleep (* 1000 dead-mixer-line-purge-time))
;        (doseq [q-id (keys @mixer-lines)]
;          (if (= 0 (.size (q-id @mixer-lines)))
;            (swap! mixer-lines dissoc q-id))))
;      (recur))))

(defn flush-mixer [] (reset! mixer-lines {})) ;;;kills all the data queues.

(def random (java.util.Random.))

(def rand-char-set ;;used to generate the keys 
  (map char (concat(range 66 91) (range 97 123))))

(defn gen-key [] 
  (keyword (apply str (take 12 (repeatedly (fn [] (nth rand-char-set (.nextInt random (count rand-char-set)))))))))

(defn add-queue-vals [] ;;;adds up the top value off of all the mixer queues.
  (let [nums (map (fn [q] (.pop q)) (remove #(= 0 (.size %)) (vals @mixer-lines)))] (reduce + nums)))

(defn generate-sine-wave
  [freq a dur]
  (let [osc-fn #(short (* a (Math/sin (* Math/PI (* 2 (/ % (/ sample-rate freq)))))))]
    (map osc-fn (range 0 (* 2 sample-rate dur)))))
;    (mapcat short-to-bytes (map osc-fn (range 0 (* 2 sample-rate dur))))))
;    (byte-array (map osc-fn (range (* sample-rate dur))))))

(def length-atom (atom [0 [0.2]]))

(defn- get-next-dur 
  []
  (if (= (@length-atom 0) (dec (count (@length-atom 1))))
    (swap! length-atom assoc 0 0)
    (swap! length-atom assoc 0 (inc (@length-atom 0))))
  (get-in @length-atom [1 (@length-atom 0)]))

(defn- test-fn
  []
  (let [s (.getLine system-mixer (first system-mixer-line-info))]
    
    (doto s 
      (.open audio-format)
      (.start))
    (doseq [f [6 7 8 9]]
      (let [ratios (map #(+ 1 (/ (* 4 %) f)) (range 0 f))
            waves (map #(generate-sine-wave (* 100 %) (/ 120 (count ratios)) (get-next-dur)) ratios)
            summed-waves (apply map + waves)]
;        (doseq [w waves]
 ;         (.write s (bytes (byte-array w)) 0 (count w)))))
        (.write s (bytes (byte-array summed-waves)) 0 (count summed-waves))))
    (doto s
      (.flush) (.stop) (.close))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;

(defn- bytes-to-unsigned-int24
  [[b1 b2 b3]]
  (bit-or (bit-shift-left
           (bit-and b3 0xFF) 16)
          (bit-shift-left
           (bit-and b2 0xFF) 8)
          (bit-and b1 0xFF)))
          

(defn- bytes-to-signed-int24
  [[b1 b2 b3]]
  (bit-or (bit-shift-left b3 16)
          (bit-shift-left
           (bit-and b2 0xFF) 8)
          (bit-and b1 0xFF)))

        
(defn read-a-wave-file
  [filename]
  (let [bytebuffer (doto (ByteBuffer/wrap
                          (Files/readAllBytes
                           (.toPath (File. filename))))
                     (.order (ByteOrder/LITTLE_ENDIAN)))
        chunk-id (apply str (repeatedly 4 #(char (.get bytebuffer))))
        chunk-size (.getInt bytebuffer)
        format (apply str (repeatedly 4 #(char (.get bytebuffer))))
        subchunk-id (apply str (repeatedly 4 #(char (.get bytebuffer))))
        subchunk-1-size (.getInt bytebuffer)
        [audio-format num-channels] (repeatedly 2 #(.getShort bytebuffer))
        [sample-rate byte-rate] (repeatedly 2 #(.getInt bytebuffer))
        [block-align bits-per-sample] (repeatedly 2 #(.getShort bytebuffer))
        subchunk-2-id (apply str (repeatedly 4 #(char (.get bytebuffer))))
        subchunk-2-size (.getInt bytebuffer)
        num-samples (/ subchunk-2-size (* num-channels (/ bits-per-sample 8)))
        b (byte-array (.remaining bytebuffer))
        s (.getLine system-mixer (first system-mixer-line-info))]
    (.get bytebuffer b)
    (let [stereo-channels (map #(bytes-to-signed-int24 %) (partition 3 b))
          left-channel (take-nth 2 stereo-channels)
          right-channel (take-nth 2 (rest stereo-channels))]
      (println (take 50 left-channel))
      (println (take 50 right-channel))) ;;;finally figured out how to decode a 24-bit pcm wav!

;    (
;      (let [[b1 b2 b3] (repeatedly 3 #(.get audio-buffer))
;            u (bytes-to-unsigned-int24 [b1 b2 b3])
;            s (bytes-to-signed-int24 [b1 b2 b3])]
;        (println [b1 b2 b3 u s])))
        
        
;        audio-format (AudioFormat. sample-rate 16 num-channels false true)]
    
;    (doto s 
;      (.open audio-format)
 ;     (.start)
  ;    (.write (.array audio-buffer) 0 (count (.array audio-buffer)))
   ;   (.flush)
    ;  (.stop)
     ; (.close))))
    ; "/home/ianblaylock/Samples/musicradar-drum-samples/Drum Kits/Kit 4 - Electro"
    (println [chunk-id chunk-size format subchunk-id subchunk-1-size
              audio-format num-channels sample-rate byte-rate block-align
              bits-per-sample subchunk-2-id subchunk-2-size num-samples])))
