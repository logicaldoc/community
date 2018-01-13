package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
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
 * This panel collects all documents details
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ImportDetailsPanel extends VLayout {

	private Layout settingsTabPanel;

	private ImportSettingsPanel settingsPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private GUIArchive archive;

	private ImportArchivesList listPanel;

	public ImportDetailsPanel(GUIArchive archive, ImportArchivesList listPanel) {
		super();
		this.listPanel = listPanel;
		this.archive = archive;
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
				ImpexService.Instance.get().load(getArchive().getId(), new AsyncCallback<GUIArchive>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIArchive archive) {
						setArchive(archive);
						savePanel.setVisible(false);
					}
				});
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

		Tab versionsTab = new Tab(I18N.message("settings"));
		settingsTabPanel = new HLayout();
		settingsTabPanel.setWidth100();
		settingsTabPanel.setHeight100();
		versionsTab.setPane(settingsTabPanel);
		tabSet.addTab(versionsTab);

		addMember(tabSet);

		refresh();
	}

	private void refresh() {
		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		/*
		 * Prepare the import settings tab
		 */
		if (settingsPanel != null) {
			settingsPanel.destroy();
			if (settingsTabPanel.contains(settingsPanel))
				settingsTabPanel.removeMember(settingsPanel);
		}
		settingsPanel = new ImportSettingsPanel(archive, changeHandler);
		settingsTabPanel.addMember(settingsPanel);
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public void onSave() {
		if (settingsPanel.validate()) {
			ImpexService.Instance.get().save(archive, new AsyncCallback<GUIArchive>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIArchive result) {
					savePanel.setVisible(false);
					listPanel.updateRecord(result);
				}
			});
		}
	}

	public GUIArchive getArchive() {
		return archive;
	}

	public void setArchive(GUIArchive archive) {
		this.archive = archive;
		refresh();
	}
}