<#escape x as (x!)?html>
	<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
	<html>
		<head>
			<style type="text/css">
				body {background-color: rgb(246,246,246); }
				h1 { color: rgb(0,51,102); font-family: helvetica; font-size: 15pt; font-weight: bold; }
				h2 { color: rgb(0,51,102); font-family: helvetica; font-size: 13pt; font-weight: bold; }
				h3 { color: rgb(0,51,102); font-family: helvetica; font-size: 9pt; font-weight: bold; }
				p { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				a { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				a.th { color: rgb(246,246,246); background-color: rgb(102,102,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				p.error { color: rgb(255,0,0); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				th.color { color: rgb(246,246,246); background-color: rgb(102,102,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				th.no_color { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				td.color { color: rgb(0,51,102); background-color: rgb(255,225,225); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				td.color_pre { color: rgb(0,51,102); background-color: rgb(255,225,225); font-family: helvetica; font-size: 10pt; font-weight: normal; white-space: pre }
				td.no_color { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				td.no_color_pre { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; white-space: pre }
				.methodBody {padding-top: 50px}
			</style>
		
			<title>
				LogicalDOC API - ${className(service.webServiceClass.name)} 
			</title>
		</head>	
		<body>
			<h1>Webservice: ${className(service.webServiceClass.name)}</h1>
			<h2>
				<#noescape>
					${classDescription(service.webServiceClass.name)}
				</#noescape>
			</h2>
			
			<table cellspacing="1" cellpadding="1" border="0">
				<tbody>
						<tr valign="top">
							<th align="left" class="color">Index</th>
							<th class="no_color"/>
							<th align="left" class="color">Method</th>				
							<th class="no_color"/>
							<th align="left" class="color">Request(s)</th>
							<th class="no_color"/>
							<th align="left" class="color">Response</th>
							<th class="no_color"/>
							<th align="left" class="color">Description</th>																												
						</tr>
					<#list service.methodStubs as method>						
						<tr class="no_color">
							<td/>
						</tr>					
						<tr valign="top">
							<td align="left" class="color"><a href="#method${method_index + 1}">${method_index + 1}</a></td>				
							<td class="no_color"/>		
							<td align="left" class="color"><a href="#method${method_index + 1}">${method.methodName}</a></td>
							<td class="no_color"/>
							<td align="left" class="color">
								<#list method.requestStubs as stub>
									 ${elementName(stub.stubName)}<br/>
								</#list>
							</td>
							<td class="no_color"/>
							<td align="left" class="color">
								<#if method.responseStub??>
									${elementName(method.responseStub.stubName)}
								</#if>
							</td>
							<td class="no_color"/>
							<td align="left" class="color">
								<#noescape>
									${method.description}
								</#noescape>
							</td>								
						</tr>
					</#list>
				</tbody>
			</table>
		    <#list service.methodStubs as method>		
		    	<div class="methodBody"> 		    		
		    		<hr/>		
					<a name="method${method_index + 1}"><h2>Method #${method_index + 1}: ${method.methodName}</h2><p>${method.description}</p></a>
						<#if (method.requestStubs?size > 0)>
							<h3>Request</h3>
							<table cellspacing="1" cellpadding="1" border="0">
								<tbody>
										<tr valign="top">
											<th align="left" class="color">Name</th>
											<th class="no_color"/>
											<#if method.inheritanceInvolved>
												<th align="left" class="color">Scope</th>
												<th class="no_color"/>
											</#if>	
											<th align="left" class="color">Type</th>
											<th class="no_color"/>
											<th align="left" class="color">Required</th>
											<th class="no_color"/>
											<th align="left" class="color">Multiple</th>
											<th class="no_color"/>
											<th align="left" class="color">Description</th>
										</tr>
			
										<#macro stubRow stub indence inheritanceInvolved>
										  <#if elementName(stub.stubName) != "sskip">
											<tr class="no_color">
												<td/>
											</tr>					
											<tr valign="top">
												<td align="left" class="color">
													<#list 0..indence as i>
													  	&nbsp;&nbsp;&nbsp;&nbsp;
													</#list>  
													${elementName(stub.stubName)}
													<br/>
												</td>		
												<#if inheritanceInvolved>		
													<td class="no_color"/>		
													<td align="left" class="color">
														<#if stub.subTypeOfParentStub??>
															Only for <b>${className(stub.subTypeOfParentStub.name)}</b>											
														</#if>
													</td>
												</#if>
												<td class="no_color"/>		
												<td align="left" class="color">
													<#noescape>														
														<#assign stubTypeName>
															${elementType(stub.type.name)}
														</#assign>
														${stubTypeName}
													</#noescape>	
												</td>
												<td class="no_color"/>		
												<td align="left" class="color">${stub.required?string("Y","")}</td>
												<td class="no_color"/>		
												<td align="left" class="color">${stub.multiOccurs?string("Y","")}</td>
												<td class="no_color"/>		
												<td align="left" class="color">
													<#noescape>
													    ${stub.description}
													</#noescape>
												</td>																									
											</tr>								 
											<#list stub.childStubs as childStub>									
												<@stubRow stub=childStub indence=indence+1 inheritanceInvolved=inheritanceInvolved/>
											</#list>								
										  </#if>
										</#macro>  		
										
										
										<#list method.requestStubs as s>								 		
											<@stubRow stub=s indence=0 inheritanceInvolved=method.inheritanceInvolved/>
										</#list>
										
			
								</tbody>
							</table>
						</#if>
			
							<#if method.responseStub??>				
								<h3>Response</h3>
								<table cellspacing="1" cellpadding="1" border="0">
									<tbody>
											<tr valign="top">
												<th align="left" class="color">Name</th>
												<th class="no_color"/>
												<#if method.inheritanceInvolved>
													<th align="left" class="color">Scope</th>
													<th class="no_color"/>
												</#if>	
												<th align="left" class="color">Type</th>
												<th class="no_color"/>
												<th align="left" class="color">Required</th>
												<th class="no_color"/>
												<th align="left" class="color">Multiple</th>
												<th class="no_color"/>
												<th align="left" class="color">Description</th>
											</tr>
											<@stubRow stub=method.responseStub indence=0 inheritanceInvolved=method.inheritanceInvolved/>							
									</tbody>
								</table>					
							</#if>
							
							<#if method.inheritanceInvolved>
								<h3>Type Hierarchy</h3>
								
								<#macro typeRow typeTree indence>
											<tr class="no_color">
												<td/>
											</tr>					
											<tr valign="top">
												<td align="left" class="color">
													<#list 0..indence as i>
													  	&nbsp;&nbsp;&nbsp;&nbsp;
													</#list>  
													${className(typeTree.type.name)}
												</td>																								
											</tr>								 
											<#list typeTree.children as childTree>									
												<@typeRow typeTree=childTree indence=indence+1/>
											</#list>								
			
								</#macro>  									
								
								<table cellspacing="1" cellpadding="1" border="0">
									<tbody>									
											<#list method.stubTypeTreeRepository.allTrees as typeTree>
												<#if !typeTree.parent??>
													<tr>								
														<@typeRow typeTree=typeTree indence=0/>
													</tr>											
												</#if>								
	
											</#list>
									</tbody>
								</table>								
							</#if>						
						 
					</div>				
			</#list>				
		</body>
	</html>  
</#escape>