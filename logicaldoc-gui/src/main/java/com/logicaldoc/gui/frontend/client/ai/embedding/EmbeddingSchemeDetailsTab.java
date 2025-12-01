package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the embedding scheme details area
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemeDetailsTab extends HLayout {

	protected EmbeddingSchemesPanel schemesPanel;

	protected GUIEmbeddingScheme embeddingScheme;

	protected ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param schemesPanel The panel showing all the schemes
	 * @param embeddingScheme The embedding scheme this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        syndication
	 */
	protected EmbeddingSchemeDetailsTab(EmbeddingSchemesPanel schemesPanel, GUIEmbeddingScheme embeddingScheme,
			ChangedHandler changedHandler) {
		super();
		this.embeddingScheme = embeddingScheme;
		this.changedHandler = changedHandler;
		this.schemesPanel = schemesPanel;
	}

	public GUIEmbeddingScheme getEmbeddingScheme() {
		return embeddingScheme;
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
