**Meditation variable description:** At the baseline assessment (T1), 
participants were asked how long they had maintained a formal meditation
practice, with two text box inputs ('years' and 'months') where they could fill in 
their response. This information was converted to a 'meditation years' value, 
reflecting how long they had meditated in years. 

Details:  

1) any text was removed, leaving only numeric data    
2) if months > 12 - response was made NA  
3) years was converted to months (years * 12 = year months)  
4) year months was added to months for total months  
5) total months was converted back to years (total months / 12 = meditation years)  

