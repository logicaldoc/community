package com.logicaldoc.gui.common.client.util;

import java.util.Map;

import com.smartgwt.client.util.ValueCallback;

/**
 * A value callback to handle multiple values 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public interface ValuesCallback extends ValueCallback {
	void execute(Map<String, Object> values);
}