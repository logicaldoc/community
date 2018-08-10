package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.google.gwt.http.client.URL;
import com.logicaldoc.gui.common.client.Session;

/**
 * GUI representation of an external call represented by an additional context
 * menu item
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class GUIExternalCall implements Serializable {
	private static final long serialVersionUID = 1L;

	private String name = "External Call";

	private String baseUrl = "http://localhost:8080";

	private String suffix;

	private String targetWindow = "_blank";

	private String[] parameters;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getBaseUrl() {
		return baseUrl;
	}

	public void setBaseUrl(String baseUrl) {
		this.baseUrl = baseUrl;
	}

	public String getSuffix() {
		return suffix;
	}

	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	public String getTargetWindow() {
		return targetWindow;
	}

	public void setTargetWindow(String targetWindow) {
		this.targetWindow = targetWindow;
	}

	public String[] getParameters() {
		return parameters;
	}

	public void setParameters(String[] parameters) {
		this.parameters = parameters;
	}

	public void setParametersStr(String params) {
		List<String> buf = new ArrayList<String>();
		String[] tokens = params.split(",");
		for (String token : tokens)
			buf.add(token.trim());
		parameters = buf.toArray(new String[0]);
	}

	public String getUrl(boolean document, Long[] ids, String[] titles) {
		String url = getBaseUrl();
		url += "?type=" + (document ? "document" : "folder");
		url += "&id=";
		for (Long id : ids) {
			if (!url.endsWith("="))
				url += ",";
			url += id.toString();
		}

		for (String param : getParameters()) {
			if ("user".equals(param.trim()))
				url += "&user=" + Session.get().getUser().getUserName();
			else if ("filename".equals(param.trim())) {
				url += "&filename=";
				for (String title : titles) {
					if (!url.endsWith("="))
						url += ",";
					url += title;
				}
			}
		}

		if (getSuffix() != null && !"".equals(getSuffix()))
			url += "&" + getSuffix();

		return URL.encode(url);
	}
}