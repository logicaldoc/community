package com.logicaldoc.gui.common.client;

import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.user.client.rpc.RpcRequestBuilder;

/**
 * Our own Request Builder that specifies a timeout (param gui.rpc.timeout)
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class LDRpcRequestBuilder extends RpcRequestBuilder {
	private static final int DEFAULT_TIMEOUT = 2;

	@Override
	protected RequestBuilder doCreate(String serviceEntryPoint) {
		int timeout = DEFAULT_TIMEOUT;
		if (Session.get().getConfig("gui.rpc.timeout") != null)
			timeout = Integer.parseInt(Session.get().getConfig("gui.rpc.timeout"));
		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST, serviceEntryPoint);
		builder.setTimeoutMillis(timeout * 60 * 1000);
		return builder;
	}
}
