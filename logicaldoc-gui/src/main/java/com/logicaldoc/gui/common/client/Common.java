package com.logicaldoc.gui.common.client;

import com.google.gwt.core.client.EntryPoint;

/**
 * The Common entry point
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Common implements EntryPoint {

	private static Common instance;

	/**
	 * @return singleton Main instance
	 */
	public static Common get() {
		return instance; 
	}

	@Override
	public void onModuleLoad() {
		instance = this;
	}
}