package com.logicaldoc.web.websockets;

import com.google.gwt.user.server.rpc.SerializationPolicy;
import com.google.gwt.user.server.rpc.SerializationPolicyProvider;

/**
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class CustomSerializationPolicyProvider implements SerializationPolicyProvider {
	@Override
	public SerializationPolicy getSerializationPolicy(String moduleBaseURL, String serializationPolicyStrongName) {
		return new SimpleSerializationPolicy();
	}
}
