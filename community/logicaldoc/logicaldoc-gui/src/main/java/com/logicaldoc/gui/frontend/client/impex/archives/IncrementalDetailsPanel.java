package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
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
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class IncrementalDetailsPanel extends VLayout implements FolderChangeListener {

	protected Layout settingsTabPanel;

	protected IncrementalSettingsPanel settingsPanel;

	protected HLayout savePanel;

	private TabSet tabSet = new TabSet();

	protected GUIIncrementalArchive incremental;

	private IncrementalArchivesList listPanel;

	public IncrementalDetailsPanel(GUIIncrementalArchive incremental, IncrementalArchivesList listPanel) {
		super();

		this.incremental = incremental;
		this.listPanel = listPanel;
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
				if (getIncremental().getId() != 0) {
					ImpexService.Instance.get().loadIncremental(getIncremental().getId(),
							new AsyncCallback<GUIIncrementalArchive>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIIncrementalArchive incremental) {
									setIncremental(incremental);
								}
							});
				} else {
					GUIIncrementalArchive archive = new GUIIncrementalArchive();
					archive.setType(IncrementalDetailsPanel.this.incremental.getType());
					setIncremental(archive);
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

		Tab versionsTab = new Tab(I18N.message("settings"));
		settingsTabPanel = new HLayout();
		settingsTabPanel.setWidth100();
		settingsTabPanel.setHeight100();
		versionsTab.setPane(settingsTabPanel);
		tabSet.addTab(versionsTab);

		addMember(tabSet);

		refresh();
	}

	public GUIIncrementalArchive getIncremental() {
		return incremental;
	}

	public void setIncremental(GUIIncrementalArchive incremental) {
		this.incremental = incremental;
		refresh();
	}

	protected void refresh() {
		if (savePanel != null)
			savePanel.setVisible(false);

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		/*
		 * Prepare the versions tab
		 */
		if (settingsPanel != null) {
			settingsPanel.destroy();
			if (settingsTabPanel.contains(settingsPanel))
				settingsTabPanel.removeMember(settingsPanel);
		}
		settingsPanel = new IncrementalSettingsPanel(incremental, changeHandler, this);
		settingsTabPanel.addMember(settingsPanel);
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public void onSave() {
		if (settingsPanel.validate()) {
			ImpexService.Instance.get().saveIncremental(incremental, new AsyncCallback<GUIIncrementalArchive>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIIncrementalArchive incremental) {
					savePanel.setVisible(false);
					if (incremental != null) {
						listPanel.updateRecord(incremental);
						listPanel.showDetails(incremental);
					}
				}
			});
		}
	}

	@Override
	public void onChanged(GUIFolder folder) {
		onModified();
	}
}