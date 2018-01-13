package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

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

	protected HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private AttributeSetsPanel setsPanel;

	public AttributeSetDetailsPanel(AttributeSetsPanel panel) {
		super();
		this.setsPanel = panel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (attributeSet.getId() != 0) {
					AttributeSetService.Instance.get().getAttributeSet(attributeSet.getId(),
							new AsyncCallback<GUIAttributeSet>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAttributeSet set) {
									setAttributeSet(set);
								}
							});
				} else {
					setAttributeSet(new GUIAttributeSet());
				}
				savePanel.setVisible(false);
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);

		tabSet = new TabSet();
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
		if (savePanel != null)
			savePanel.setVisible(false);

		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (propertiesTabPanel.contains(propertiesPanel))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		propertiesPanel = new AttributeSetPropertiesPanel(attributeSet, changeHandler, this);
		propertiesTabPanel.addMember(propertiesPanel);
	}

	public GUIAttributeSet getAttributeSet() {
		return attributeSet;
	}

	public void setAttributeSet(GUIAttributeSet set) {
		this.attributeSet = set;
		refresh();
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public HLayout getSavePanel() {
		return savePanel;
	}

	protected boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	protected void onSave() {
		if (validate()) {
			AttributeSetService.Instance.get().save(attributeSet, new AsyncCallback<GUIAttributeSet>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIAttributeSet result) {
					savePanel.setVisible(false);
					setsPanel.updateRecord(result);
					setAttributeSet(result);
				}
			});
		}
	}
}
