package com.logicaldoc.web.firewall;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.web.firewall.FirewalledRequest;
import org.springframework.security.web.firewall.RequestRejectedException;
import org.springframework.security.web.firewall.StrictHttpFirewall;

import com.logicaldoc.util.Context;

/**
 * A firewall specialization that allows to be turned off.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 * 
 */
public class HttpFirewall extends StrictHttpFirewall {

	public HttpFirewall() {
		super();
	}

	@Override
	public FirewalledRequest getFirewalledRequest(HttpServletRequest request) throws RequestRejectedException {
		if (Context.get().getProperties().getBoolean("firewall.enabled", true))
			return super.getFirewalledRequest(request);
		else
			return new FirewalledRequest(request) {

				@Override
				public void reset() {
				}
			};
	}

	@Override
	public HttpServletResponse getFirewalledResponse(HttpServletResponse response) {
		if (Context.get().getProperties().getBoolean("firewall.enabled", true))
			return super.getFirewalledResponse(response);
		else
			return response;
	}
}
