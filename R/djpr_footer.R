#' DJPR website footer
#'
#' @param ... unused
#'
#' @return HTML element
#' @export
djpr_footer <- function(...){
  HTML(
"<footer>
<link rel=\"stylesheet\" type=\"text/css\" href=\"djprshiny/footer.css\"/>
<div class=\"nrs\">
<div class=\"restrict-width\">
Deaf, hearing or speech impaired? Contact the National Relay Service: Call 1800 555 660 or visit <a href=\"http://www.relayservice.com.au\">www.relayservice.com.au</a>
</div>
</div>


<div class=\"restrict-width\">
<div class=\"utility-links print-hidden\">
<span><a href=\"https://djpr.vic.gov.au/disclaimer\">Disclaimer</a></span>
<span><a href=\"https://djpr.vic.gov.au/accessibility\">Accessibility</a></span>
<span><a href=\"https://djpr.vic.gov.au/sitemap\">Sitemap</a></span>
<span><a href=\"https://djpr.vic.gov.au/privacy\">Privacy</a></span>
<span><a href=\"https://djpr.vic.gov.au/copyright\">Copyright</a></span>
<span><a href=\"https://djpr.vic.gov.au/about-us/contact-us\">Contact us</a></span>
</div>

<div class=\"social-media print-hidden\">
<a class=\"linkedin\" href=\"https://www.linkedin.com/company/department-of-jobs-precincts-and-regions/?viewAsMember=true\"><span class=\"visually-hidden\">Linkedin</span></a>
<a class=\"twitter\" href=\"https://twitter.com/VicGovDJPR\"><span class=\"visually-hidden\">Twitter</span></a>
<a class=\"youtube\" href=\"https://www.youtube.com/channel/UCLQPr8_met-tfidLe5jREsA\"><span class=\"visually-hidden\">YouTube</span></a>
</div>

<div class=\"copyright\">
<p>Department of Jobs, Precincts and Regions</p>
<p>Copyright 2022</p>
</div>
</div>
</footer>"
  )
}
