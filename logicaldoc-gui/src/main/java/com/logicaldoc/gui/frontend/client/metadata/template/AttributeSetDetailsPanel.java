package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all attribute set details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetDetailsPanel extends VLayout {

	protected GUIAttributeSet attributeSet;

	protected Layout propertiesTabPanel;

	protected AttributeSetPropertiesPanel propertiesPanel;

	protected EditingTabSet tabSet;

	private AttributeSetsPanel setsPanel;

	public AttributeSetDetailsPanel(AttributeSetsPanel panel) {
		super();
		this.setsPanel = panel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (attributeSet.getId() != 0) {
				AttributeSetService.Instance.get().getAttributeSet(attributeSet.getId(),
						new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIAttributeSet set) {
								setAttributeSet(set);
							}
						});
			} else {
				setAttributeSet(new GUIAttributeSet());
			}
		});
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		addMember(tabSet);
	}

	protected void refresh() {
		disableSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		ChangedHandler changeHandler = event -> onModified();

		propertiesPanel = new AttributeSetPropertiesPanel(attributeSet, changeHandler, this);
		propertiesTabPanel.addMember(propertiesPanel);
	}

	void disableSave() {
		tabSet.hideSave();
	}

	void enableSave() {
		tabSet.displaySave();
	}

	public void onModified() {
		enableSave();
	}

	public GUIAttributeSet getAttributeSet() {
		return attributeSet;
	}

	public void setAttributeSet(GUIAttributeSet set) {
		this.attributeSet = set;
		refresh();
	}

	protected boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	protected void onSave() {
		if (validate()) {
			AttributeSetService.Instance.get().save(attributeSet, new AsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIAttributeSet result) {
					setsPanel.updateRecord(result);
					setAttributeSet(result);
				}
			});
		}
	}
}