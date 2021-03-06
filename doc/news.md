### FLIBM 0.3.3

---

##### New features
      - `cohort-FLIBM` added. This simplified simulation function can be used
      alone to simulate a single cohort. The resulting object can be returned.
      `ypr.FLIBM` now serves as a wrapper for this function, allowing one to 
      explore the effect of variable fishing mortality on a cohort.
      - Minor changes to vignette building and README.md. 


<br><br>


### FLIBM 0.3.2

---

##### New features
      - `flquant2flq` replaces FLIBM2lfq. Input is now an FLQuant object
      for more flexibility.


<br><br>


### FLIBM 0.3.1

---

##### New features
      - `fmsy.FLIBM` temporary results directory (`resDir`) uses R session-
      designated temporary directory (`tempdir()`)
      - Length at maximum cohort biomass (*Lopt*) added to output of 
      `ypr.FLIBM`


<br><br>


### FLIBM 0.3.0

---

##### New features
      - Major change: FLIBM object now holds inds within a single level list
      (`obj$inds`) with `obj$inds$unit` and `obj$inds$area` as individual
      variables. `iter` dimension is no longer used as multiple iterations
      drastically slow down the simulation. It is now recommended that multiple
      iterations be done in individual objects (and multiple R instances), 
      whose FLQuant objects can be combined later. The new formulation will
      more easily allow for the definition of unit and area specific functions 
      (e.g. sex-specific growth & mortality etc.)


<br><br>


### FLIBM 0.2.0

---

##### New features
      - Default `create.FLIBM` and `grow.inds` functions were changed to 
      differentiate between population growth parameters 
      (e.g. `popLinf`, `popK`) and individual parameters. 
      This will save memory in the `$inds` object by not recording 
      parameters for which there is no variation among individuals.


<br><br>


### FLIBM 0.1.2

---

##### New features
      - calcRefs function now calculates virgin SSB based on average of lowest 
      FM values. This allows for additional flexibility when addressing 
      stocasticity among similar values of maximum fishing mortality (`FM`).
      - `war.FLIBM` function added, allowing of the estimation of the width of 
      a given cohort's length distribution through time (i.e. width between 
      defined quantiles). The width at recruitment (WAR) is defined as the 
      width of the cohort's length distribution when the lower quantile 
      surpasses the length of recruitment to the fishery (`L50`).


<br><br>

### FLIBM 0.1.1

---

##### New features
      - Small changes to handling of seed values in functions dealing with 
      reference point calculation and plotting. 


<br><br>


### FLIBM 0.1.0

---

##### New features
      - Additional functionality of recruitment processes. Allows for lagged
      recruitment (`obj$rec$lag`), initial length (`obj$growth$params$L0`, 
      `obj$growth$params$L0.cv`)
      - obj carries `make.inds` function for added flexibility in defining 
      alternate model structure
      - new functions `FLIBM.window` and `FLIBM.trim` for changing dimensions of 
      FLIBM objects
      - new `obj$rec$rec` slot for recording recruitment values; necessary in 
      the case of lagged recruitment
      - `refptPlot` updated
      - inds.R now contains all operational functions
      - `adv.FLIBM` arguments renamed for clarity, and contains additional 
      update.inds step following recruitment; necessary to update states of 
      newly recruited inds (i.e. mortality)
      
##### Documentation
      - Both manuals ("FLIBM_Manual", "Assessment_with_FLIBM_objects") are 
      updated


<br><br>

### FLIBM 0.0.3

---

##### New features
      - ypr.FLIBM function added for estimating Fmax, F01, and corresponding
      SPR (including parallel computing option).
      - fmsy.FLIBM function added for estimating Fmsy and corresponding
      SPR (including parallel computing option).
      - Slight change to new recruited individuals. These are immediately 
      vulnerable to mortality, and their states are now 
      recorded at the time step.
      - calcFM function allows estimation of FM for a given catch quota.
      
##### Documentation
      - Both manuals ("FLIBM_Manual", "Assessment_with_FLIBM_objects") are 
      updated

##### Bug fixes
      - Simulations are now reproducible with setting of seed(). Issue with 
      inds cleanup process in remove.inds() fixed.

<br><br>

