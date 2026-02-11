package com.logicaldoc.gui.frontend.client.ai.filler;

import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the filler details area
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public abstract class FillerDetailsTab extends HLayout {
	protected final GUIFiller filler;

	protected final ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param filler The filler this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        filler
	 */
	protected FillerDetailsTab(GUIFiller filler, ChangedHandler changedHandler) {
		super();
		this.filler = filler;
		this.changedHandler = changedHandler;
	}

	public GUIFiller getFiller() {
		return filler;
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