package com.logicaldoc.gui.frontend.client.system.update;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIPatch;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ApplicationRestarting;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.frontend.client.services.UpdateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Patches check panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class PatchPanel extends VLayout {

	// Shows the install notes panel
	VLayout notesPanel = new VLayout();

	// Shows the download panel
	VLayout downloadPanel = new VLayout();

	// Shows the lis of patches
	VLayout listPanel = new VLayout();

	private IButton download = new IButton(I18N.message("download"));

	private IButton cancel = new IButton(I18N.message("cancel"));

	public PatchPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		if (!Feature.enabled(Feature.PATCHES)) {
			setMembers(new FeatureDisabled(Feature.PATCHES));
			return;
		}

		showList();
	}

	private void switchListView(GUIPatch[] patches) {
		if (contains(downloadPanel))
			removeMember(downloadPanel);

		ListGridField id = new ListGridField("id", I18N.message("id"), 100);
		id.setHidden(true);

		ListGridField file = new ListGridField("file", I18N.message("file"), 150);
		file.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("patch"), 110);

		ListGridField installed = new ListGridField("installed", I18N.message("installed"), 110);
		installed.setType(ListGridFieldType.BOOLEAN);

		ListGridField restart = new ListGridField("restart", I18N.message("requiresrestart"), 110);
		restart.setType(ListGridFieldType.BOOLEAN);

		ListGridField rating = new ListGridField("rating", I18N.message("severityrating"), 110);
		rating.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				int rating = 0;
				if (value != null)
					rating = Integer.parseInt(value.toString());

				return "<span style='color: " + GUIPatch.getColor(rating) + "'>"
						+ I18N.message("severityrating." + rating) + "</span>";
			}
		});

		ListGridField date = new DateListGridField("date", I18N.message("date"), DateCellFormatter.FORMAT_SHORT);

		ListGridField size = new FileSizeListGridField("size", I18N.getAttributeLabel("size"));

		size.setCanFilter(false);

		final ListGrid list = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (!record.getAttributeAsBoolean("installed"))
					return super.getCellCSSText(record, rowNum, colNum);
				else
					return "color: #888888; font-style: italic;";
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setCanExpandRecords(true);
		list.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		list.setDetailField("description");
		list.setFields(id, name, rating, date, size, installed, restart, file);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu(list);
				event.cancel();
			}
		});

		List<ListGridRecord> records = new ArrayList<ListGridRecord>();
		if (patches != null && patches.length > 0) {
			for (GUIPatch patch : patches) {
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("id", patch.getId());
				record.setAttribute("name", patch.getName());
				record.setAttribute("rating", patch.getRating());
				record.setAttribute("file", patch.getFile());
				record.setAttribute("date", patch.getDate());
				record.setAttribute("size", patch.getSize());
				record.setAttribute("description", patch.getDescription());
				record.setAttribute("installed", patch.isInstalled());
				record.setAttribute("restart", patch.isRestart());
				records.add(record);
			}
			list.setRecords(records.toArray(new ListGridRecord[0]));
		}

		listPanel = new VLayout();
		listPanel.setWidth100();
		listPanel.setHeight100();
		listPanel.setMembers(list);
		addMember(listPanel);
	}

	private void switchDownloadView(GUIPatch patch) {
		if (contains(listPanel))
			removeMember(listPanel);

		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px", "*");
		form.setTitleOrientation(TitleOrientation.LEFT);

		StaticTextItem name = ItemFactory.newStaticTextItem("name", "name", patch.getName());
		name.setRequired(true);
		name.setWrapTitle(false);

		StaticTextItem rating = ItemFactory.newStaticTextItem("rating", "severityrating", "<span style='color: "
				+ patch.getColor() + "'>" + I18N.message("severityrating." + patch.getRating()) + "</span>");
		rating.setRequired(true);
		rating.setWrapTitle(false);

		StaticTextItem date = ItemFactory.newStaticTextItem("date", "date", I18N.formatDateShort(patch.getDate()));
		date.setRequired(true);
		date.setWrapTitle(false);

		StaticTextItem size = ItemFactory.newStaticTextItem("size", "size", Util.formatSize(patch.getSize()));
		size.setRequired(true);
		size.setWrapTitle(false);

		StaticTextItem restart = ItemFactory.newStaticTextItem("restart", "requiresrestart",
				patch.isRestart() ? I18N.message("yes") : I18N.message("no"));
		restart.setRequired(true);
		restart.setWrapTitle(false);

		StaticTextItem description = ItemFactory.newStaticTextItem("description", "description",
				patch.getDescription());
		description.setWidth(500);
		description.setRequired(true);
		description.setWrapTitle(false);

		form.setItems(name, date, size, restart, description);

		Label message = new Label();
		message.setContents(I18N.message("installpatch", patch.getName()));
		message.setWrap(false);
		message.setAlign(Alignment.LEFT);
		message.setStyleName("updateavailable");
		message.setLayoutAlign(Alignment.LEFT);
		message.setLayoutAlign(VerticalAlignment.TOP);
		message.setHeight(20);

		VLayout download = prepareDownloadProgress(patch);

		VLayout infoPanel = new VLayout();
		infoPanel.setMembersMargin(10);
		infoPanel.setMembers(form, download);

		HLayout body = new HLayout();
		body.setWidth100();
		body.setMembersMargin(50);
		notesPanel.setWidth100();
		body.setMembers(infoPanel, notesPanel);

		downloadPanel = new VLayout();
		downloadPanel.setWidth100();
		downloadPanel.setHeight100();
		downloadPanel.setMembers(message, body);

		addMember(downloadPanel);
	}

	private void showList() {
		LD.contactingServer();

		UpdateService.Instance.get().checkPatch(Session.get().getInfo().getUserNo(),
				Session.get().getInfo().getRelease(), new AsyncCallback<GUIPatch[]>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIPatch[] patches) {
						LD.clearPrompt();
						switchListView(patches);
					}
				});
	}

	private VLayout prepareDownloadProgress(GUIPatch patch) {
		final String fileName = patch.getFile();

		VLayout layout = new VLayout(4);
		layout.setWidth100();

		final Label barLabel = new Label(I18N.message("downloadprogress"));
		barLabel.setHeight(16);
		barLabel.setWrap(false);
		layout.addMember(barLabel);

		final Progressbar bar = new Progressbar();
		bar.setHeight(24);
		bar.setVertical(false);
		bar.setLength(300);
		layout.addMember(bar);

		final IButton confirmPatch = new IButton(I18N.message("confirmpatch"));
		confirmPatch.setAutoFit(true);
		confirmPatch.setVisible(false);
		confirmPatch.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				SC.ask(I18N.message("confirmpatch"), I18N.message("confirmpatchquestion"), new BooleanCallback() {

					@Override
					public void execute(Boolean choice) {
						if (choice.booleanValue()) {
							confirmPatch.setVisible(false);
							download.setVisible(false);
							UpdateService.Instance.get().confirmPatch(fileName, new AsyncCallback<String>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(String path) {
									cancel.setVisible(false);

									if (!patch.isRestart()) {
										ApplicationRestarting
												.get(I18N.message("patchrunning", path.replaceAll("\\\\", "/"))).show();
									} else {
										ApplicationRestarting
												.get(I18N.message("patchrunning1", path.replaceAll("\\\\", "/")))
												.show();

										final String tenant = Session.get().getUser().getTenant().getName();
										Session.get().close();
										CookiesManager.removeSid();

										Timer timer = new Timer() {
											public void run() {
												Util.waitForUpAndRunning(tenant, I18N.getLocale());
											}
										};
										timer.schedule(30000);
									}
								}
							});
						}
					}
				});
			}
		});

		cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				showList();
			}
		});

		download = new IButton(I18N.message("download"));
		download.setAutoFit(true);
		download.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				bar.setPercentDone(0);
				download.setDisabled(true);

				UpdateService.Instance.get().downloadPatch(Session.get().getInfo().getUserNo(), patch.getId(), fileName,
						patch.getSize(), new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
								download.setDisabled(false);
							}

							@Override
							public void onSuccess(Void arg) {
								confirmPatch.setVisible(false);

								new Timer() {
									public void run() {
										UpdateService.Instance.get().checkDownloadStatus(new AsyncCallback<int[]>() {

											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(int[] status) {
												bar.setPercentDone(status[1]);

												if (status[1] == 100) {
													download.setDisabled(false);

													confirmPatch.setVisible(true);
													displayNotes(fileName);
												} else
													schedule(50);
											}
										});
									}
								}.schedule(50);
							}
						});
			}
		});

		HLayout buttonCanvas = new HLayout();
		buttonCanvas.setMembersMargin(6);
		buttonCanvas.setMembers(cancel, download, confirmPatch);

		layout.addMember(buttonCanvas);

		return layout;
	}

	private void displayNotes(String fileName) {
		UpdateService.Instance.get().getPatchNotes(fileName, new AsyncCallback<String[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(String[] infos) {
				DynamicForm form = new DynamicForm();
				form.setTitleOrientation(TitleOrientation.TOP);
				form.setColWidths("*");
				form.setNumCols(1);

				TextAreaItem changelog = ItemFactory.newTextAreaItem("changelog", "changelog", infos[0]);
				changelog.setWidth("100%");
				changelog.setHeight(220);

				TextAreaItem patchNotes = ItemFactory.newTextAreaItem("patchnotes", "patchnotes", infos[1]);
				patchNotes.setWidth("100%");
				patchNotes.setHeight(220);

				form.setItems(patchNotes, changelog);

				notesPanel.addMember(form);
			}
		});
	}

	private void showContextMenu(ListGrid list) {
		ListGridRecord record = list.getSelectedRecord();
		final GUIPatch patch = new GUIPatch();
		patch.setId(record.getAttribute("id"));
		patch.setName(record.getAttribute("name"));
		patch.setFile(record.getAttribute("file"));
		patch.setDescription(record.getAttribute("description"));
		patch.setSize(record.getAttributeAsLong("size"));
		patch.setDate(record.getAttributeAsDate("date"));
		patch.setInstalled(record.getAttributeAsBoolean("installed"));

		Menu contextMenu = new Menu();

		MenuItem install = new MenuItem();
		install.setTitle(I18N.message("install"));
		install.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				switchDownloadView(patch);
			}
		});
		install.setEnabled(!patch.isInstalled());

		contextMenu.setItems(install);
		contextMenu.showContextMenu();
	}
}