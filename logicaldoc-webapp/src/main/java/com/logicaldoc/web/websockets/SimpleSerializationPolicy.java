package com.logicaldoc.web.websockets;

import java.io.Serializable;

import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.server.rpc.SerializationPolicy;

/**
 * RPC generates a serialization policy file during GWT compilation. The
 * serialization policy file contains a whitelist of allowed types which may be
 * serialized. In this simple implementation there is only the check of the
 * Serializable interface. Watch out of what are you serializing or you can
 * perform problem on client side.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 *
 */
public class SimpleSerializationPolicy extends SerializationPolicy {

	@Override
	public boolean shouldDeserializeFields(Class<?> clazz) {
		return isSerializable(clazz);
	}

	@Override
	public boolean shouldSerializeFields(Class<?> clazz) {
		return isSerializable(clazz);
	}

	private boolean isSerializable(Class<?> clazz) {
		if (clazz != null) {
			if (clazz.isPrimitive() || Serializable.class.isAssignableFrom(clazz)
					|| IsSerializable.class.isAssignableFrom(clazz)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void validateDeserialize(Class<?> arg0) throws SerializationException {
		// Nothing to do
	}

	@Override
	public void validateSerialize(Class<?> arg0) throws SerializationException {
		// Nothing to do
	}
}