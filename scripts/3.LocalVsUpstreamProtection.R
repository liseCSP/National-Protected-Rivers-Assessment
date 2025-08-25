#---------------------------------------------------------
#Analysis in/out protection

#Lise Comte, August 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: https://figshare.com/s/62d2f6da57b9526a9aee

#----------------------------------------------------
#Estimate protection per state
library(dplyr); library(stringr); library(sf)

# ---- Load data ----
net_protect_seg_fin <- read.csv("outputs/Table_protection_segments_RIPAllCombined.csv")

# pad with zeros and drop missing HUC12
net_protect_seg_fin <- net_protect_seg_fin %>%
  mutate(
    HUC_12 = str_pad(as.character(HUC_12), width = 12, pad = "0"),
    HUC6   = substr(HUC_12, 1, 6),
  ) %>%
  filter(!is.na(HUC_12))

# ---- Reference hydro data ----
#Available at https://www.epa.gov/waterdata/nhdplus-national-data
ref <- st_read('data/NHDPlusNationalData/NationalWBDSnapshot.gdb',
               'WBDSnapshot_National') %>%
  st_drop_geometry() %>%
  mutate(HUC6 = substr(HUC_12, 1, 6))

# ---- Main loop over states ----
ST <- net_protect_seg_fin %>%
  filter(!State %in% c("Alaska","Hawaii")) %>%
  pull(State) %>% unique()

PROTECTION <- NULL
PROTECTION_out <- NULL

for (i in ST) {
  # Select data for this state
  ns <- net_protect_seg_fin[net_protect_seg_fin$State == i, , drop = FALSE]
  
  # In-state protection
  prot_in <- aggregate(
    . ~ RIP_Class + State,
    data = ns[, c("OverallProtection_Len_m", "Total_Length_m", "RIP_Class", "State")],
    sum,
    na.action = na.pass
  )
  prot_in$IN <- i
  
  # HUC6 in this state
  H6 <- unique(ns$HUC6)
  
  # Out-of-state protection in the same basins
  ouns <- net_protect_seg_fin[net_protect_seg_fin$HUC6 %in% H6 & net_protect_seg_fin$State != i, , drop = FALSE]
  prot_out <- aggregate(
    . ~ RIP_Class + State,
    data = ouns[, c("OverallProtection_Len_m", "Total_Length_m", "RIP_Class", "State")],
    sum,
    na.action = na.pass
  )
  prot_out$IN <- i
  
  # Candidate HUC6s outside the state (drop bogus ID)
  H6out <- setdiff(unique(ouns$HUC6), "NANANANANANA")
  
  # Collect upstream, out-of-state HUC12s (within each shared HUC6)
  upstream_connect <- character(0)
  
  for (ii in seq_along(H6out)) {
    # Segments connected to this out-of-state HUC6
    ns6 <- ns[ns$HUC6 == H6out[ii], , drop = FALSE]
    
    # All HUC12 in-state for this HUC6
    hus12_in <- unique(ns6$HUC_12)
    
    # Traverse upstream within the same HUC6
    for (iii in seq_along(hus12_in)) {
      from <- unique(ref$HUC_12[ref$HU_12_DS == hus12_in[iii] & ref$HUC6 == H6out[ii]])
      it <- 0
      while (length(from) > 0 && it < 10000) {  # guard against loops
        upstream_connect <- c(upstream_connect, from)
        from <- unique(ref$HUC_12[ref$HU_12_DS %in% from & ref$HUC6 == H6out[ii]])
        it <- it + 1
      }
    }
  }
  
  upstream_connect <- unique(upstream_connect)
  
  # Out-of-state upstream protection (only if we found any upstream HUC12s)
  if (length(upstream_connect) > 0) {
    ouns_us <- net_protect_seg_fin[
      net_protect_seg_fin$HUC_12 %in% upstream_connect & net_protect_seg_fin$State != i,
      ,
      drop = FALSE
    ]
    
    if (nrow(ouns_us) > 0) {
      prot_out_us <- aggregate(
        . ~ RIP_Class + State,
        data = ouns_us[, c("OverallProtection_Len_m", "Total_Length_m", "RIP_Class", "State")],
        sum,
        na.action = na.pass
      )
      prot_out_us$IN <- i
      
      # Append results
      PROTECTION <- rbind(PROTECTION, prot_out_us, prot_in)
      PROTECTION_out <- rbind(PROTECTION_out, prot_out)
    } else {
      # No upstream rows found — still record in-state and basin out-of-state
      PROTECTION <- rbind(PROTECTION,            prot_in)
      PROTECTION_out <- rbind(PROTECTION_out,    prot_out)
    }
  } else {
    # No upstream connections — still record in-state and basin out-of-state
    PROTECTION <- rbind(PROTECTION,              prot_in)
    PROTECTION_out <- rbind(PROTECTION_out,      prot_out)
  }
}

# ---- Add Alaska & Hawaii (no upstream connections)
ns <- net_protect_seg_fin[which(net_protect_seg_fin$State == "Hawaii"),]
prot_in <- aggregate(. ~ RIP_Class + State,sum,data=ns[,c("OverallProtection_Len_m","Total_Length_m","RIP_Class","State")],na.action = na.pass)
prot_in$IN <- "Hawaii"
PROTECTION <- rbind(PROTECTION,prot_in)

ns <- net_protect_seg_fin[which(net_protect_seg_fin$State == "Alaska"),]
prot_in <- aggregate(. ~ RIP_Class + State,sum,data=ns[,c("OverallProtection_Len_m","Total_Length_m","RIP_Class","State")],na.action = na.pass)
prot_in$IN <- "Alaska"
PROTECTION <- rbind(PROTECTION,prot_in)

# ---- Export ----
write.csv(PROTECTION,"outputs/Protection_states_InOut_huc12.csv", row.names = FALSE)  
#write.csv(PROTECTION_out,"outputs/Protection_states_allOut_huc12.csv", row.names = FALSE)  

#---------------------------------------------------
#Estimate protection type (US and per state)
library(dplyr); library(stringr); library(sf)

# ---- Load data & select viable protection only ----
net_protect_seg_fin <- read.csv("outputs/Table_protection_segments_RIPAllCombined.csv") %>%
  filter(RIP_Class %in% c("Class 1","Class 2","Class 3"))

# ---- Assign protection types by hand or use management type from PAD-US (already extracted) ----
net_protect_seg_fin <- net_protect_seg_fin %>%
  mutate(
    ONRW_Mng_Typ          = ifelse(ONRW_Len_m > 0, "State", NA),
    NWS_Mng_Typ           = ifelse(NWS_Len_m > 0, "Federal", NA),
    SWS_Mng_Typ           = ifelse(SWS_Len_m > 0, "State", NA),
    NWS_Eli_Mng_Typ       = ifelse(NWS_Eli_Len_m > 0, "Federal", NA),
    OTRW_Mng_Typ          = ifelse(OTRW_Len_m > 0, "American Indian Lands", NA),
    CriticalHabitatESA_Mng_Typ = ifelse(CritHabESA_Len_m > 0, "Federal", NA),
    BufferRiparian_Mng_Typ = case_when(
      BufferRiparian_Len_m > 0 & BufferRiparian_Des_Tp == "NWFP_RiparianReserves" ~ "Federal",
      BufferRiparian_Len_m > 0 ~ "State"
    ),
    KW_Mng_Typ            = ifelse(KW_Len_m > 0, "Federal", NA),
    HUC_12                = str_pad(as.character(HUC_12), width = 12, pad = "0") #pad with 0
  ) %>%
  filter(!is.na(HUC_12)) %>%
  mutate(
    HUC6  = substr(HUC_12, 1, 6), #create field for river basins
  )

# ---- Reference hydro data ----
#Available at https://www.epa.gov/waterdata/nhdplus-national-data
ref <- st_read('data/NHDPlusNationalData/NationalWBDSnapshot.gdb',
               'WBDSnapshot_National') %>%
  st_drop_geometry() %>%
  mutate(HUC6 = substr(HUC_12, 1, 6))

# ---- Loop States (except Alaska and Hawaii)
ST <- setdiff(unique(na.omit(net_protect_seg_fin$State)), c("Alaska", "Hawaii"))

PROTECTION <- NULL
PROTECTION_out <- NULL

for (i in ST) {
  # Select data for each state
  ns <- net_protect_seg_fin[net_protect_seg_fin$State == i, ]
  
  # In-state management
  Manag <- data.frame(
    Length  = c(ns$ONRW_Len_m, ns$NWS_Len_m, ns$SWS_Len_m, ns$NWS_Eli_Len_m,
                ns$OTRW_Len_m, ns$FPA_Len_m, ns$RFPA_Len_m, ns$WPA_Len_m,
                ns$FAM_Len_m, ns$CritHabESA_Len_m, ns$BufferRiparian_Len_m, ns$KW_Len_m,
                ns$IUCNI_Len_m, ns$IUCNII_Len_m, ns$IUCNIII_Len_m,
                ns$IUCNVI_Len_m, ns$IUCNV_Len_m, ns$IUCNVI_Len_m,
                ns$IUCNOTH_Len_m, ns$Gap3_Len_m),
    Mng_Typ = c(ns$ONRW_Mng_Typ, ns$NWS_Mng_Typ, ns$SWS_Mng_Typ, ns$NWS_Eli_Mng_Typ,
                ns$OTRW_Mng_Typ, ns$FPA_Mng_Typ, ns$RFPA_Mng_Typ, ns$WPA_Mng_Typ,
                ns$FAM_Mng_Typ, ns$CriticalHabitatESA_Mng_Typ, ns$BufferRiparian_Mng_Typ,
                ns$KW_Mng_Typ, ns$IUCNI_Mng_Typ, ns$IUCNII_Mng_Typ, ns$IUCNIII_Mng_Typ,
                ns$IUCNVI_Mng_Typ, ns$IUCNV_Mng_Typ, ns$IUCNVI_Mng_Typ,
                ns$IUCNOTH_Mng_Typ, ns$Gap3_Mng_Typ),
    State   = i
  )
  
  Manag <- na.omit(Manag)
  Manag$Mng_Typ[Manag$Mng_Typ == "STAT"] <- "State"
  
  prot_in <- aggregate(. ~ Mng_Typ + State, data = Manag, sum, na.action = na.pass)
  prot_in$IN <- i
  
  # Identify HUC6 and out-of-state basins
  H6    <- unique(ns$HUC6)
  ouns  <- net_protect_seg_fin[net_protect_seg_fin$HUC6 %in% H6 & net_protect_seg_fin$State != i, ]
  H6out <- setdiff(unique(ouns$HUC6), "NANANANANANA")
  
  # Upstream connections
  upstream_connect <- character(0)
  for (h6 in H6out) {
    ns6      <- ns[ns$HUC6 == h6, ]
    hus12_in <- unique(ns6$HUC_12)
    
    for (h12 in hus12_in) {
      from <- unique(ref$HUC_12[ref$HU_12_DS == h12 & ref$HUC6 == h6])
      it   <- 0
      while (length(from) > 0) {
        it <- it + 1
        upstream_connect <- c(upstream_connect, from)
        from <- unique(ref$HUC_12[ref$HU_12_DS %in% from & ref$HUC6 == h6])
        if (it > 10000) break  # breaks if loop
      }
    }
  }
  
  # Out-of-state upstream management
  upstream_connect <- unique(upstream_connect)
  ouns_us <- net_protect_seg_fin[
    net_protect_seg_fin$HUC_12 %in% upstream_connect & net_protect_seg_fin$State != i, ]
  
  Manag_out <- data.frame(
    Length  = c(ouns_us$ONRW_Len_m, ouns_us$NWS_Len_m, ouns_us$SWS_Len_m, ouns_us$NWS_Eli_Len_m,
                ouns_us$OTRW_Len_m, ouns_us$FPA_Len_m, ouns_us$RFPA_Len_m, ouns_us$WPA_Len_m,
                ouns_us$FAM_Len_m, ouns_us$CritHabESA_Len_m, ouns_us$BufferRiparian_Len_m, ouns_us$KW_Len_m,
                ouns_us$IUCNI_Len_m, ouns_us$IUCNII_Len_m, ouns_us$IUCNIII_Len_m,
                ouns_us$IUCNVI_Len_m, ouns_us$IUCNV_Len_m, ouns_us$IUCNVI_Len_m,
                ouns_us$IUCNOTH_Len_m, ouns_us$Gap3_Len_m),
    Mng_Typ = c(ouns_us$ONRW_Mng_Typ, ouns_us$NWS_Mng_Typ, ouns_us$SWS_Mng_Typ, ouns_us$NWS_Eli_Mng_Typ,
                ouns_us$OTRW_Mng_Typ, ouns_us$FPA_Mng_Typ, ouns_us$RFPA_Mng_Typ, ouns_us$WPA_Mng_Typ,
                ouns_us$FAM_Mng_Typ, ouns_us$CriticalHabitatESA_Mng_Typ, ouns_us$BufferRiparian_Mng_Typ,
                ouns_us$KW_Mng_Typ, ouns_us$IUCNI_Mng_Typ, ouns_us$IUCNII_Mng_Typ, ouns_us$IUCNIII_Mng_Typ,
                ouns_us$IUCNVI_Mng_Typ, ouns_us$IUCNV_Mng_Typ, ouns_us$IUCNVI_Mng_Typ,
                ouns_us$IUCNOTH_Mng_Typ, ouns_us$Gap3_Mng_Typ),
    State   = ouns_us$State
  )
  
  Manag_out <- na.omit(Manag_out)
  Manag_out$Mng_Typ[Manag_out$Mng_Typ == "STAT"] <- "State"
  
  prot_out_us <- aggregate(. ~ Mng_Typ + State, data = Manag_out, sum, na.action = na.pass)
  prot_out_us$IN <- i
  
  # Combine results
  PROTECTION <- rbind(PROTECTION, prot_out_us, prot_in)
  
  print(i)
}

#add Hawaii and Alaska
ns <- net_protect_seg_fin[which(net_protect_seg_fin$State == "Hawaii"),]
Manag <- data.frame(Length = c(ns$ONRW_Len_m,ns$NWS_Len_m,ns$SWS_Len_m,ns$NWS_Eli_Len_m,
                               ns$OTRW_Len_m,ns$FPA_Len_m,ns$RFPA_Len_m,ns$WPA_Len_m,
                               ns$FAM_Len_m,ns$CritHabESA_Len_m,ns$BufferRiparian_Len_m,ns$KW_Len_m,
                               ns$IUCNI_Len_m,ns$IUCNII_Len_m,ns$IUCNIII_Len_m,
                               ns$IUCNVI_Len_m,ns$IUCNV_Len_m,ns$IUCNVI_Len_m,ns$IUCNOTH_Len_m,ns$Gap3_Len_m),
                    Mng_Typ = c(ns$ONRW_Mng_Typ,ns$NWS_Mng_Typ,ns$SWS_Mng_Typ,ns$NWS_Eli_Mng_Typ,ns$OTRW_Mng_Typ,
                                ns$FPA_Mng_Typ,ns$RFPA_Mng_Typ,ns$WPA_Mng_Typ,ns$FAM_Mng_Typ,
                                ns$CriticalHabitatESA_Mng_Typ,ns$BufferRiparian_Mng_Typ,ns$KW_Mng_Typ,ns$IUCNI_Mng_Typ,ns$IUCNII_Mng_Typ,
                                ns$IUCNIII_Mng_Typ,ns$IUCNVI_Mng_Typ,ns$IUCNV_Mng_Typ,ns$IUCNVI_Mng_Typ,ns$IUCNOTH_Mng_Typ,ns$Gap3_Mng_Typ),
                    State = rep("Hawaii",20))

Manag <- na.omit(Manag)
Manag$Mng_Typ <- ifelse(Manag$Mng_Typ == "STAT","State",Manag$Mng_Typ)
prot_in <- aggregate(. ~ Mng_Typ + State,sum,data=Manag,na.action = na.pass)
prot_in$IN <- "Hawaii"
PROTECTION <- rbind(PROTECTION,prot_in)

ns <- net_protect_seg_fin[which(net_protect_seg_fin$State == "Alaska"),]
Manag <- data.frame(Length = c(ns$ONRW_Len_m,ns$NWS_Len_m,ns$SWS_Len_m,ns$NWS_Eli_Len_m,
                               ns$OTRW_Len_m,ns$FPA_Len_m,ns$RFPA_Len_m,ns$WPA_Len_m,
                               ns$FAM_Len_m,ns$CritHabESA_Len_m,ns$BufferRiparian_Len_m,ns$KW_Len_m,
                               ns$IUCNI_Len_m,ns$IUCNII_Len_m,ns$IUCNIII_Len_m,
                               ns$IUCNVI_Len_m,ns$IUCNV_Len_m,ns$IUCNVI_Len_m,ns$IUCNOTH_Len_m,ns$Gap3_Len_m),
                    Mng_Typ = c(ns$ONRW_Mng_Typ,ns$NWS_Mng_Typ,ns$SWS_Mng_Typ,ns$NWS_Eli_Mng_Typ,ns$OTRW_Mng_Typ,
                                ns$FPA_Mng_Typ,ns$RFPA_Mng_Typ,ns$WPA_Mng_Typ,ns$FAM_Mng_Typ,
                                ns$CriticalHabitatESA_Mng_Typ,ns$BufferRiparian_Mng_Typ,ns$KW_Mng_Typ,ns$IUCNI_Mng_Typ,ns$IUCNII_Mng_Typ,
                                ns$IUCNIII_Mng_Typ,ns$IUCNVI_Mng_Typ,ns$IUCNV_Mng_Typ,ns$IUCNVI_Mng_Typ,ns$IUCNOTH_Mng_Typ,ns$Gap3_Mng_Typ),
                    State = rep("Alaska",20))
Manag <- na.omit(Manag)
Manag$Mng_Typ <- ifelse(Manag$Mng_Typ == "STAT","State",Manag$Mng_Typ)
prot_in <- aggregate(. ~ Mng_Typ + State,sum,data=Manag,na.action = na.pass)
prot_in$IN <- "Alaska"
PROTECTION <- rbind(PROTECTION,prot_in)

write.csv(PROTECTION,"outputs/Management_states_InOut_huc12.csv")  

#--------------------------------------------
#Barplot with extent of protection and protection type combined 
#------------------------------------------
library(reshape2); library(tidyverse); library(ggplot2); library(vcd)

#get viable protection (in %)
PROTECTION <- read.csv("outputs/Protection_states_InOut_huc12.csv")

#Format data
names(PROTECTION)[names(PROTECTION)=="IN"] <- "To"
names(PROTECTION)[names(PROTECTION)=="State"] <- "From"

PROTECTION$Dir <- paste(PROTECTION$From,PROTECTION$To,sep="_")
PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")

inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)

OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)

inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(names(outOfState),names(OeOut))]
pp <- data.frame(Protection = c(-inState,outOfState),State = c(names(inState),names(outOfState)),Status=c(rep("In-State",length(inState)),rep("Out-State",length(outOfState))))

#get protection type
MANAG <- read.csv("outputs/Management_states_InOut_huc12.csv")

#reduce number of categories
MANAG$Mng_Typ <- ifelse(MANAG$Mng_Typ %in% c("Joint","Regional Agency Special District","Unknown","American Indian Lands"),"Other",MANAG$Mng_Typ)
MANAG$Mng_Typ <- ifelse(MANAG$Mng_Typ %in% c("Non-Governmental Organization","Private"),"Private",MANAG$Mng_Typ)
MANAG$Mng_Typ <- ifelse(MANAG$Mng_Typ %in% c("Local Government"),"Local",MANAG$Mng_Typ)

names(MANAG)[names(MANAG)=="IN"] <- "To"
names(MANAG)[names(MANAG)=="State"] <- "From"

MANAG$Dir <- paste(MANAG$From,MANAG$To,sep="_")
MANAG$INOUT <-  ifelse(MANAG$From == MANAG$To,"in","out")
MANAG$Mng_Typ <- factor(MANAG$Mng_Typ)
OR <- names(inState[order(inState)])
MANAG$To <- factor(MANAG$To,levels=OR)

F_stat_state <- aggregate(Length ~ To + Mng_Typ,sum,data=MANAG[which(MANAG$INOUT == "in"),],drop=F,na.action=na.pass)
F_stat_stateT <- aggregate(Length ~ To,sum,data=MANAG[which(MANAG$INOUT == "in"),],na.action=na.pass)

#save overall contributions (before transformation to %)
state_plot_save <- acast(F_stat_state, To~Mng_Typ, value.var="Length")
state_plot_save <- ifelse(is.na(state_plot_save)==T,0,state_plot_save)
manag_all <- round(apply(state_plot_save,2,sum)*100/sum(state_plot_save),2)
manag_conus <- round(apply(state_plot_save[!rownames(state_plot_save) %in% c("Alaska","Hawaii"),],2,sum)*100/sum(state_plot_save[!rownames(state_plot_save) %in% c("Alaska","Hawaii")]),2)
dataf <- data.frame(manag_all,manag_conus)
write.csv(dataf,"outputs/TableS3_Summary_protection_type.csv")

F_stat_state$protect <- F_stat_state$Length*100/F_stat_stateT$Length[match(F_stat_state$To,F_stat_stateT$To)]
F_stat_state$INOUT = "in"

F_stat_stateOut <- aggregate(Length ~ To + Mng_Typ,sum,data=MANAG[which(MANAG$INOUT == "out"),],drop=F,na.action=na.pass)
F_stat_stateTOut <- aggregate(Length ~ To,sum,data=MANAG[which(MANAG$INOUT == "out"),],na.action=na.pass)

F_stat_stateOut$protect <- F_stat_stateOut$Length*100/F_stat_stateTOut$Length[match(F_stat_stateOut$To,F_stat_stateTOut$To)]
F_stat_stateOut$INOUT = "out"

state_plot <- acast(F_stat_state, To~Mng_Typ, value.var="protect")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)

state_plotOut <- acast(F_stat_stateOut, To~Mng_Typ, value.var="protect")
state_plotOut <- ifelse(is.na(state_plotOut)==T,0,state_plotOut)

#rescale according to overall extent of viable protection
state_plot <- state_plot * c(inState[match(rownames(state_plot),names(inState))])/100
state_plotOut <- state_plotOut * c(outOfState[match(rownames(state_plotOut),names(outOfState))])/100

#little trick to have negative values for out-of-state
state_plotOut <- state_plotOut*-1

state_plot <- state_plot[nrow(state_plot):1,]
state_plotOut <- state_plotOut[nrow(state_plotOut):1,]

state_plot <- state_plot[,c("Federal","State","Local","Private","Other")]
state_plotOut <- state_plotOut[,c("Federal","State","Local","Private","Other")]

#remove District of columbia for the figure
state_plot <- state_plot[- which(rownames(state_plot) == "District of Columbia"),]
state_plotOut <- state_plotOut[-which(rownames(state_plotOut) == "District of Columbia"),]

cols_bar <- c("#8B633DFF", "darkolivegreen", "#C4B57CFF", "#CB4E33FF", "#AECCDFFF")


jpeg("outputs/BarplotStateInOutManagment.jpeg", units="in", width=18, height=13, res=300, pointsize = 19.5)
par(mar=c(1,4,1,0))
b=barplot(t(state_plot),horiz=F,las=1,ylab="",cex.names=0.9,col=rev(cols_bar),ylim=c(-60,60),legend=F,cex.lab=1.4,las=2,axes=F,border=rev(cols_bar),names=rep(NA,length(rownames(state_plot))))#
barplot(t(state_plotOut),horiz=F,col=alpha(rev(cols_bar),0.5),axes=F,border=alpha(rev(cols_bar),0.5),names=rep(NA,length(rownames(state_plot))),add=T)#
par(xpd=NA)
text(b-0.6,apply(state_plot,1,sum)+1,colnames(t(state_plot)),srt=90,pos=4,hjust=0)
axis(2,las=3,pos=-0.5,cex.axis=1.2,at=c(-60,-40,-20,0,20,40,60),labels=c(60,40,20,0,20,40,60))
mtext(side=2,"Protected river length (%)",cex=1.5,line=1.8)
arrows(x0=61.5,y0=0,x1=61.5,y1=60,length=0.1,lwd=2)
arrows(x0=61.5,y0=0,x1=61.5,y1=-60,length=0.1,col=grey(0.4),lwd=2)
text(60.5, 50, labels = 'In-state', xpd = NA, srt = -270,cex=1.2,font=3)
text(60.5, -48, labels = 'Out-of-state', xpd = NA, srt = -270,cex=1.2,font=3,col=grey(0.4))

#add custom legend
g = grid_legend(0.5, 0.25, pch=15, col=rev(cols_bar),
                frame = F,
                size=2,
                hgap = 0.1,
                vgap=1,
                labels=(colnames(state_plot)),
                gp_title = gpar(cex = 1.2),
                gp_labels = gpar(cex = 1),
                title = "Protection type", draw=FALSE)

grid.draw(grobTree(g, vp=viewport(x=0.75,y=0.2,angle=-270)))

dev.off()

#------------------------------------------------------------
# Figure with all connections among states
#sankey diagram - viable protection across states

library(circlize); library(reshape2); library(googleVis); library(networkD3); library(tidyverse); library(viridis)
library(patchwork); library(hrbrthemes); library(webshot); library(Polychrome); library(vcd)

PROTECTION <- read.csv("data/Protection_states_InOut_huc12.csv")
names(PROTECTION)[names(PROTECTION)=="IN"] <- "To"
names(PROTECTION)[names(PROTECTION)=="State"] <- "From"

PROTECTION$Dir <- paste(PROTECTION$From,PROTECTION$To,sep="_")
PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")

#percentage of state-specific protection
#river miles viable protection for each connection from --> to
P <- tapply(PROTECTION$OverallProtection_Len_m[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],PROTECTION$Dir[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],sum)

#percentage of viable protection in-state versus out of state
inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$Dir[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)

OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)#total river miles in state
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)#total river miles coming from other state

inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(sapply(strsplit(names(outOfState),"_"),'[',2),names(OeOut))]

df <- data.frame(from=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',1)),to=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',2)),length_Protected = c(inState,outOfState))

#reorder according to contribution
df <- df[order(df$length_Protected,decreasing=T),]
 
#reorder both in and out using the same order
ff <- ifelse(df$to==df$from,1,0)#identify in state connections

le <- df$from[ff==1] #set levels
df$from <- factor(df$from,levels=le)
nff1 <- order(df$from[ff==1])
nff2 <- order(df$from[ff==0])
df <- rbind(df[ff==1,][nff1,],df[ff==0,][nff2,])

#little trick to order the nodes
df$to <- paste(df$to, " ")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(df$from), as.character(df$to)) %>% unique())
nodes$node_group <- gsub(" ","_", nodes$name) #spaces are not allowed

# With networkD3, connection must be provided using id, not using real name like in the links dataframe, so we need to reformat it
df$IDsource <- match(df$from, nodes$name)-1 
df$IDtarget <- match(df$to, nodes$name)-1
df$group <- as.factor(gsub(" ","_",df$from))

# prepare colour scale
Pstate = createPalette(51,  c("#332288", "#44AA99", "#661100"))
colors <- paste(Pstate,collapse = '", "')
ColourScal <- paste('d3.scaleOrdinal(["', colors, '"])')

#ITERATIONS STOPS THE ALGORITHM FROM REORDERING
sk1 <- sankeyNetwork(Links = df, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "length_Protected", NodeID = "name",NodeGroup = "node_group", 
                     sinksRight=FALSE, nodeWidth=40, fontSize=24, nodePadding=2,LinkGroup="group",iterations = 0,colourScale=ColourScal) 
sk1

#push labels outside the plot
sk1$x$nodes <-
  sk1$x$nodes %>% 
  mutate(is_source_node = name %in% df$from)

sk1 <- htmlwidgets::onRender(
  sk1,
  '
  function(el,x) {
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return d.is_source_node; })
    .attr("x", x.options.nodeWidth - 50)
    .attr("text-anchor", "end");
  
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return !d.is_source_node; })
    .attr("x", x.options.nodeWidth +5)
    .attr("text-anchor", "start");
  }
  '
)
sk1

# save it as an html
saveNetwork(sk1, "outputs/SankeyProtection_huc12.html")

# convert it as jpeg
webshot("outputs/SankeyProtection_huc12.html","outputs/SankeyProtection_huc12.jpeg", vwidth = 1800, vheight = 2200, zoom = 3)


#----------------------------------------------------------------
#Example Colorado River Basin
#---------------------------------------------------------------
PROTECTION <- read.csv("data/Protection_CRBstates_InOut_huc12.csv") #obtained using same script as before but with network cropped to HUC02 - 14 & 15
names(PROTECTION)[names(PROTECTION)=="IN"] <- "To"
names(PROTECTION)[names(PROTECTION)=="State"] <- "From"
PROTECTION$Dir <- paste(PROTECTION$From,PROTECTION$To,sep="_")


#--------------------------------------------------------
#sankey diagram CRB states - viable protection

#percentage of state-specific protection
#river miles viable protection for each connection from --> to
P <- tapply(PROTECTION$OverallProtection_Len_m[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],PROTECTION$Dir[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],sum)

#Option 3: percentage of viable protection in-state versus out of state
PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")

inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$Dir[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)

OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)#total river miles in state
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)#total river miles coming from other state

inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(sapply(strsplit(names(outOfState),"_"),'[',2),names(OeOut))]

df <- data.frame(from=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',1)),to=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',2)),length_Protected = c(inState,outOfState))

#reorder both in and out using the same order
ff <- ifelse(df$to==df$from,1,0)#identify in state connections

le <- df$from[ff==1] #set levels
df$from <- factor(df$from,levels=le)
nff1 <- order(df$from[ff==1])
nff2 <- order(df$from[ff==0])
df <- rbind(df[ff==1,][nff1,],df[ff==0,][nff2,])

#little trick to order the nodes
df$to <- paste(df$to, " ")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(df$from), as.character(df$to)) %>% unique())
nodes$node_group <- gsub(" ","_", nodes$name) #spaces are not allowed

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
df$IDsource <- match(df$from, nodes$name)-1 
df$IDtarget <- match(df$to, nodes$name)-1
df$group <- as.factor(gsub(" ","_",df$from))

# prepare colour scale
Pstate <- c("#332288", "#44AA99", "#661100","#FF9200","#FFCD73","#7BA246","#D37681")

colors <- paste(Pstate,collapse = '", "')
ColourScal <- paste('d3.scaleOrdinal(["', colors, '"])')

#ITERATIONS STOPS THE ALGORITHM FROM REORDERING
sk1 <- sankeyNetwork(Links = df, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "length_Protected", NodeID = "name",NodeGroup = "node_group", 
                     sinksRight=FALSE, nodeWidth=40, fontSize=24, nodePadding=2,LinkGroup="group",iterations = 0,colourScale=ColourScal) 
sk1

#push labels outside the plot
sk1$x$nodes <-
  sk1$x$nodes %>% 
  mutate(is_source_node = name %in% df$from)

sk1 <- htmlwidgets::onRender(
  sk1,
  '
  function(el,x) {
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return d.is_source_node; })
    .attr("x", x.options.nodeWidth - 50)
    .attr("text-anchor", "end");
  
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return !d.is_source_node; })
    .attr("x", x.options.nodeWidth +5)
    .attr("text-anchor", "start");
  }
  '
)
sk1

# save it as an html
saveNetwork(sk1, "outputs/SankeyProtection_CRB.html")

# convert it as jpeg
webshot("outputs/SankeyProtection_CRB.html","outputs/SankeyProtection_CRB.jpeg", vwidth = 1000, vheight = 700, zoom = 3)


#--------------------------------------------
#Inset summary

df$INOUT <-  ifelse(df$from == df$to,"in","out")
df$to <- factor(df$to,levels=rev(levels(factor(df$to))))
df$from <- factor(df$from,levels=rev(levels(factor(df$from))))

tapply(df$length_Protected[df$INOUT=="out"],df$to[df$INOUT=="out"],sum)

tapply(df$length_Protected[df$INOUT=="in"],df$to[df$INOUT=="in"],sum)

state_plotOut <- acast(df[df$INOUT=="out",], to~from, value.var="length_Protected")
state_plotOut <- ifelse(is.na(state_plotOut)==T,0,state_plotOut*-1)

state_plot <- acast(df[df$INOUT=="in",], to~from, value.var="length_Protected")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)

Pstate <- c("#332288", "#44AA99", "#661100","#FF9200","#FFCD73","#7BA246","#D37681")

jpeg("outputs/Inset_CRB.jpeg", units="in", width=4, height=2, res=300, pointsize = 9.5)
par(mar=c(.5,0.5,1.7,0.5))
b=barplot(t(state_plot),horiz=T,las=1,ylab="",cex.names=0.9,col=alpha(rev(Pstate),0.9),xlim=c(-50,50),legend=F,cex.lab=1.4,las=2,axes=F,border="transparent",names=rep(NA,length(rownames(state_plot))))#
barplot(t(state_plotOut),horiz=T,col=alpha(rev(Pstate),0.4),axes=F,border="transparent",names=rep(NA,length(rownames(state_plot))),add=T)#
abline(v=0)
par(xpd=NA)
#text(apply(state_plot,1,sum)+1,b,colnames(t(state_plot)),srt=0,pos=4,hjust=0)
axis(3,las=1,pos=8.5,cex.axis=1.2,at=c(-50,-25,0,25,50),labels=c(50,25,0,25,50))
dev.off()
