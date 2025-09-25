package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the sampler details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public abstract class SamplerDetailsTab extends HLayout {
	protected GUISampler sampler;

	protected ChangedHandler changedHandler;

	/**
	 * 
	 * @param sampler The sampler this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        syndication
	 */
	protected SamplerDetailsTab(GUISampler sampler, ChangedHandler changedHandler) {
		super();
		this.sampler = sampler;
		this.changedHandler = changedHandler;
	}

	public GUISampler getSampler() {
		return sampler;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}