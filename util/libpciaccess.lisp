;; (C) 2011 Pierre-Yves Baccou

;; This was useful for my unfinished NVidia HW cursor hack.

(require 'cffi)

(cffi:define-foreign-library libpciaccess
    (:unix "libpciaccess.so")
  (t (:default "libpciaccess")))
(cffi:use-foreign-library libpciaccess)

(defparameter nullp (cffi:null-pointer))

(cffi:defctype pciaddr :uint32)

(cffi:defcfun ("pci_system_init" %system-init) :int)

(cffi:defcfun ("pci_device_probe" %device-probe) :int
  (dev :pointer))

(cffi:defcfun ("pci_device_enable" %device-enable) :void
  (dev :pointer))

(cffi:defcfun ("pci_device_find-by-slot" %device-find-by-slot) :pointer
  (domain :uint32)
  (bus :uint32)
  (dev :uint32)
  (func :uint32))

(cffi:defcfun ("pci_device_map_range" %device-map-range) :int
  (dev :pointer)
  (base :pciaddr)
  (size :pciaddr)
  (flags :uint)
  (addr :pointer))


(defun system-init ()
  (%system-init))

(defun device-probe (dev)
  (setf dev

(defstruct device
  domain bus dev func
  vendor-id device-id
  subvendor-id subdevice-id
  device-class revision
  (regions nil) rom-size irq
  (user-data nil)) ;  unused


(defstruct mem-region
  memory bus_addr base_addr size
  is-IO is-prefetchable is-64)
  
(defun %dev-l2c (dev ptr)
  (setf (cffi:foreign-slot-value ptr 'device 'domain) (device-domain dev))
  (setf (cffi:foreign-slot-value ptr 'device 'bus) (device-bus dev))
  (setf (cffi:foreign-slot-value ptr 'device 'dev) (device-dev dev))
  (setf (cffi:foreign-slot-value ptr 'device 'func) (device-func dev))
  (setf (cffi:foreign-slot-value ptr 'device 'vendor-id) (device-vendor-id dev))
  (setf (cffi:foreign-slot-value ptr 'device 'device-id) (device-device-id dev))
  (setf (cffi:foreign-slot-value ptr 'device 'subvendor-id) (device-subvendor-id dev))
  (setf (cffi:foreign-slot-value ptr 'device 'device-id) (device-subdevice-id dev))
  (setf (cffi:foreign-slot-value ptr 'device 'device-class) (device-device-class dev))
  (setf (cffi:foreign-slot-value ptr 'device 'revision) (device-revision dev))
  (setf (cffi:foreign-slot-value ptr 'device 'rom-size) (device-rom-size dev))
  (setf (cffi:foreign-slot-value ptr 'device 'rom-size) (device-rom-size dev))
  (setf (cffi:foreign-slot-value ptr 'device 'user-data) (cffi:nullp)))

(defun %dev-c2l (dev ptr)
  (setf (device-domain dev) (cffi:foreign-slot-value ptr 'device 'domain))
  (setf (device-bus dev) (cffi:foreign-slot-value ptr 'device 'bus))
  (setf (device-dev dev) (cffi:foreign-slot-value ptr 'device 'dev))
  (setf (device-func dev) (cffi:foreign-slot-value ptr 'device 'func))
  (setf (device-vendor-id dev) (cffi:foreign-slot-value ptr 'device 'vendor-id))
  (setf (device-device-id dev) (cffi:foreign-slot-value ptr 'device 'device-id))
  (setf (device-subvendor-id dev) (cffi:foreign-slot-value ptr 'device 'subvendor-id))
  (setf (device-subdevice-id dev) (cffi:foreign-slot-value ptr 'device 'device-id))
  (setf (device-device-class dev) (cffi:foreign-slot-value ptr 'device 'device-class))
  (setf (device-revision dev) (cffi:foreign-slot-value ptr 'device 'revision))
  (setf (device-rom-size dev) (cffi:foreign-slot-value ptr 'device 'rom-size))
  (setf (device-rom-size dev) (cffi:foreign-slot-value ptr 'device 'rom-size)))



struct pci_mem_region {
    void *memory;
    pciaddr_t bus_addr;
    pciaddr_t base_addr;
    pciaddr_t size;
    unsigned is_IO:1;
    unsigned is_prefetchable:1;
    unsigned is_64:1;
};


struct pci_device {
    uint16_t    domain;
    uint8_t     bus;
    uint8_t     dev;
    uint8_t     func;
    uint16_t    vendor_id;
    uint16_t    device_id;
    uint16_t    subvendor_id;
    uint16_t    subdevice_id;
    uint32_t    device_class;
    uint8_t     revision;
    struct pci_mem_region regions[6];
    pciaddr_t   rom_size;
    int irq;
    intptr_t user_data;
};


  
;-----------------------------------------------------------------
; unix examples
       
(cffi:defcstruct posix-timeval
  (sec :int)
  (usec :int))

; each bit of the fdset controls the fd of the same number. (ie to listen to fd 3, bit 3 = 1)
(defun create-fdset (fds)
  (let ((fdset (cffi:foreign-alloc :uint8 :initial-element 0 :count (/ +fd-setsize+ 8))))
    (dolist (fd fds)
      (let ((fdelt (floor fd 8))
	    (fdmask (ash 1 (mod fd 8))))
	(setf (cffi:mem-aref fdset :uint8 fdelt)
	      (logior (cffi:mem-aref fdset :uint8 fdelt) fdmask))))
    fdset))
