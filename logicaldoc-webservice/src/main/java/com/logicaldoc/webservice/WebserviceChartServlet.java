package com.logicaldoc.webservice;

import com.logicaldoc.web.ChartServlet;

/**
 * This servlet provides the chart of the calls to the API
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class WebserviceChartServlet extends ChartServlet {

	private static final long serialVersionUID = -6956612970433309888L;

	/**
	 * Constructor of the object.
	 */
	public WebserviceChartServlet() {
		super();
		super.prefix = WebserviceInterceptor.WSCALL;
		super.rowKey = "calls";
	}
}