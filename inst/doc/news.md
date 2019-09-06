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

