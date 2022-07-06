<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>

<!--add loading indicator while the app is being loaded-->
<body dir="<%=dir%>">
<div id="loadingwrapper-<%=MODULE%>">
	<div id="loading">
		<div class="loadingIndicator">
			<img src="<%=MODULE%>/sc/skins/<%=SKIN%>/images/loading.gif" width="16" height="16"
				 style="margin-right: 8px; float: left; vertical-align: top;" />
				<span id="loadingTitle"></span>
					<br />
				<span id="loadingMsg">Loading styles and images...</span>
		</div>
	</div>
</div>
</body>
<script type="text/javascript">
	document.getElementById('loadingTitle').innerHTML = 'Loading';
</script>
<script type="text/javascript">
	document.getElementById('loadingMsg').innerHTML = 'Loading Core API...';
</script>

<!--include the SC Core API-->
<script src="<%=MODULE%>/sc/modules/ISC_Core.js?isc_version=7.1.js"></script>

<!--include SmartClient -->
<script type="text/javascript">
	document.getElementById('loadingMsg').innerHTML = 'Loading UI Components...';
</script>
<script src='<%=MODULE%>/sc/modules/ISC_Foundation.js'></script>
<script src='<%=MODULE%>/sc/modules/ISC_Containers.js'></script>
<script src='<%=MODULE%>/sc/modules/ISC_Grids.js'></script>
<script src='<%=MODULE%>/sc/modules/ISC_Forms.js'></script>
<script src='<%=MODULE%>/sc/modules/ISC_RichTextEditor.js'></script>
<script src='<%=MODULE%>/sc/modules/ISC_Calendar.js'></script>
<script type="text/javascript">
	document.getElementById('loadingMsg').innerHTML = 'Loading Data API...';
</script>
<script src='<%=MODULE%>/sc/modules/ISC_DataBinding.js'></script>

<!--load skin-->
<script type="text/javascript">document.getElementById('loadingMsg').innerHTML = 'Loading skin...';</script>
<script type="text/javascript">
    document.write("<"+"script src=<%=MODULE%>/sc/skins/<%=SKIN%>/load_skin.js?isc_version=7.1.js&tenant=<%=TENANT%>"+"><"+"/script>");
</script>

<!--load localizations-->
<script type="text/javascript">
	document.getElementById('loadingMsg').innerHTML = '';	
</script>

<!--include the nocache JS-->
<script type="text/javascript" src='<%=MODULE%>/<%=MODULE%>.nocache.js'></script>