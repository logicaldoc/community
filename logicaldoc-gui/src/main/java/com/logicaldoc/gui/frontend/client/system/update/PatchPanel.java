package com.logicaldoc.gui.frontend.client.system.update;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
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
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Patches check panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class PatchPanel extends VLayout {

	private static final String LOCAL = "local";

	private static final String DESCRIPTION = "description";

	private static final String RATING = "rating";

	private static final String RESTART = "restart";

	private static final String INSTALLED = "installed";

	private static final long MAX_WAIT_TIME = 2L * 60L * 1000L; // 2 minutes

	// Shows the install notes panel
	VLayout notesPanel = new VLayout();

	// Shows the download panel
	VLayout downloadPanel = new VLayout();

	// Shows the lis of patches
	VLayout listPanel = new VLayout();

	private IButton download = new IButton(I18N.message("download"));

	private IButton cancel = new IButton(I18N.message("cancel"));

	private IButton delete = new IButton(I18N.message("ddelete"));

	private IButton confirmPatch = new IButton(I18N.message("confirmpatch"));

	private IButton upload = new IButton(I18N.message("uploadpatch"));

	private ButtonItem ok;

	private TextAreaItem log;

	private Date lastConfirmed = null;

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

	private void switchListView(List<GUIPatch> patches) {
		Util.removeChildren(this);
		Util.removeChildren(notesPanel);

		ListGridField id = new ListGridField("id", I18N.message("id"), 100);
		id.setHidden(true);

		ListGridField file = new ListGridField("file", I18N.message("file"), 150);
		file.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("patch"), 110);

		ListGridField installed = new ListGridField(INSTALLED, I18N.message(INSTALLED), 100);
		installed.setType(ListGridFieldType.BOOLEAN);

		ListGridField restart = new ListGridField(RESTART, I18N.message("requiresrestart"), 110);
		restart.setType(ListGridFieldType.BOOLEAN);

		ListGridField rating = new ListGridField(RATING, I18N.message("severityrating"), 110);
		rating.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			int ratingVal = 0;
			if (value != null)
				ratingVal = Integer.parseInt(value.toString());

			return "<span style='color: " + GUIPatch.getColor(ratingVal) + "'>"
					+ I18N.message("severityrating." + ratingVal) + "</span>";
		});

		ListGridField date = new DateListGridField("date", I18N.message("date"), DateCellFormatter.FORMAT_SHORT);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));

		ListGridField local = new ListGridField(LOCAL, I18N.message(LOCAL), 70);
		local.setType(ListGridFieldType.BOOLEAN);
		local.setHidden(true);

		size.setCanFilter(false);

		final ListGrid list = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (Boolean.FALSE.equals(rec.getAttributeAsBoolean(INSTALLED)))
					return super.getCellCSSText(rec, rowNum, colNum);
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
		list.setDetailField(DESCRIPTION);
		list.setFields(id, name, rating, date, size, installed, restart, file, local);

		list.addCellContextClickHandler(contextClick -> {
			showContextMenu(list);
			contextClick.cancel();
		});

		List<ListGridRecord> records = new ArrayList<>();
		for (GUIPatch patch : patches) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", patch.getId());
			rec.setAttribute("name", patch.getName());
			rec.setAttribute(RATING, patch.getRating());
			rec.setAttribute("file", patch.getFile());
			rec.setAttribute("date", patch.getDate());
			rec.setAttribute("size", patch.getSize());
			rec.setAttribute(DESCRIPTION, patch.getDescription());
			rec.setAttribute(INSTALLED, patch.isInstalled());
			rec.setAttribute(RESTART, patch.isRestart());
			rec.setAttribute(LOCAL, patch.isLocal());
			records.add(rec);
		}
		list.setRecords(records.toArray(new ListGridRecord[0]));

		listPanel = new VLayout();
		listPanel.setMembersMargin(3);
		listPanel.setWidth100();
		listPanel.setHeight100();
		listPanel.setMembers(list);
		listPanel.addMember(upload);
		addMember(listPanel);

		upload.setAutoFit(true);
		upload.addClickHandler(click -> new PatchUploader(this).show());
	}

	private void switchDetailView(GUIPatch patch) {
		Util.removeChildren(this);

		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px", "*");
		form.setTitleOrientation(TitleOrientation.LEFT);

		StaticTextItem name = ItemFactory.newStaticTextItem("name", patch.getName());
		name.setRequired(true);
		name.setWrapTitle(false);

		StaticTextItem rating = ItemFactory.newStaticTextItem(RATING, "severityrating", "<span style='color: "
				+ patch.getColor() + "'>" + I18N.message("severityrating." + patch.getRating()) + "</span>");
		rating.setRequired(true);
		rating.setWrapTitle(false);

		StaticTextItem date = ItemFactory.newStaticTextItem("date", I18N.formatDateShort(patch.getDate()));
		date.setRequired(true);
		date.setWrapTitle(false);

		StaticTextItem size = ItemFactory.newStaticTextItem("size", Util.formatSize(patch.getSize()));
		size.setRequired(true);
		size.setWrapTitle(false);

		StaticTextItem requiresRestart = ItemFactory.newStaticTextItem(RESTART, "requiresrestart",
				patch.isRestart() ? I18N.message("yes") : I18N.message("no"));
		requiresRestart.setRequired(true);
		requiresRestart.setWrapTitle(false);

		form.setItems(name, date, size, requiresRestart);

		Label message = new Label();
		message.setContents(I18N.message("installpatch", patch.getName()));
		message.setWrap(false);
		message.setAlign(Alignment.LEFT);
		message.setStyleName("updateavailable");
		message.setLayoutAlign(Alignment.LEFT);
		message.setLayoutAlign(VerticalAlignment.TOP);
		message.setHeight(20);

		VLayout downloadLayout = prepareActionBar(patch);

		VLayout infoPanel = new VLayout();
		infoPanel.setMembersMargin(10);
		infoPanel.setMembers(form, downloadLayout);

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

	private void switchLogView(GUIPatch patch) {
		Util.removeChildren(this);

		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("*");
		form.setTitleOrientation(TitleOrientation.TOP);

		log = ItemFactory.newTextAreaItem("log", "");
		log.setWidth("*");

		ok = new ButtonItem("back", I18N.message("ok"));
		ok.addClickHandler(event -> onOk(patch));
		ok.setDisabled(true);

		form.setItems(log, ok);

		addMember(form);

		getStatus(patch);
	}

	private void onOk(GUIPatch patch) {
		if (patch.isRestart())
			Util.waitForUpAndRunning(Session.get().getTenantName(), I18N.getLocale());
		else
			showList();
	}

	public void showList() {
		LD.contactingServer();

		UpdateService.Instance.get().checkPatch(new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIPatch> patches) {
				LD.clearPrompt();
				switchListView(patches);
			}
		});
	}

	private VLayout prepareActionBar(GUIPatch patch) {
		final String fileName = patch.getFile();

		VLayout layout = new VLayout(4);
		layout.setWidth100();

		final Label barLabel = new Label(I18N.message("downloadprogress"));
		barLabel.setHeight(16);
		barLabel.setWrap(false);
		layout.addMember(barLabel);
		barLabel.setVisible(!patch.isLocal());

		final Progressbar bar = new Progressbar();
		bar.setHeight(24);
		bar.setVertical(false);
		bar.setLength(300);
		bar.setVisible(!patch.isLocal());
		layout.addMember(bar);

		confirmPatch.setAutoFit(true);
		confirmPatch.setVisible(patch.isLocal());
		confirmPatch.addClickHandler(event -> onConfirm(patch));

		cancel.setAutoFit(true);
		cancel.addClickHandler(event -> showList());

		delete.setAutoFit(true);
		delete.addClickHandler(event -> onDelete(fileName));
		download.setVisible(patch.isLocal());

		download.setAutoFit(true);
		download.setVisible(!patch.isLocal());
		download.addClickHandler(event -> {
			bar.setPercentDone(0);
			download.setDisabled(true);

			UpdateService.Instance.get().downloadPatch(patch.getId(), fileName, patch.getSize(), new AsyncCallback<>() {

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
							UpdateService.Instance.get().checkDownloadStatus(new AsyncCallback<>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(List<Integer> status) {
									bar.setPercentDone(status.get(1));

									if (status.get(1) == 100) {
										download.setDisabled(false);
										confirmPatch.setVisible(true);
										delete.setVisible(true);
										displayNotes(fileName);
									} else
										schedule(50);
								}
							});
						}
					}.schedule(50);
				}
			});
		});

		HLayout buttonCanvas = new HLayout();
		buttonCanvas.setMembersMargin(6);
		buttonCanvas.setMembers(cancel, download, confirmPatch, delete);

		layout.addMember(buttonCanvas);

		if (patch.isLocal())
			displayNotes(fileName);

		return layout;
	}

	private void onDelete(String fileName) {
		SC.ask(I18N.message("delete"), I18N.message("deletepatchquestion"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				UpdateService.Instance.get().deletePatch(fileName, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						showList();
					}
				});
			}
		});
	}

	private void displayNotes(String fileName) {
		UpdateService.Instance.get().getPatchNotes(fileName, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<String> infos) {
				VLayout panel = new VLayout();

				Label notesLabel = new Label(I18N.message("patchnotes"));
				notesLabel.setWrap(false);
				notesLabel.setAutoFit(true);
				notesLabel.setHeight(20);
				notesLabel.setStyleName("update-notes");
				String notesContent = infos.get(1);
				if (!notesContent.contains("<p") && !notesContent.contains("<div") && !notesContent.contains("<br")
						&& !notesContent.contains("<span") && !notesContent.contains("<table"))
					notesContent = notesContent.replace("\n", "<br>");

				HTMLPane notesContentPanel = new HTMLPane();
				notesContentPanel.setWidth100();
				notesContentPanel.setHeight("50%");
				notesContentPanel.setShowEdges(true);
				notesContentPanel.setContents(notesContent);
				notesContentPanel.setContentsType(ContentsType.FRAGMENT);

				Label changesLabel = new Label(I18N.message("changelog"));
				changesLabel.setWrap(false);
				changesLabel.setAutoFit(true);
				changesLabel.setHeight(20);
				changesLabel.setStyleName("update-changelog");
				String changesContent = infos.get(0);
				if (!changesContent.contains("<p") && !changesContent.contains("<div")
						&& !changesContent.contains("<br") && !changesContent.contains("<span")
						&& !changesContent.contains("<table"))
					changesContent = changesContent.replace("\n", "<br>");

				HTMLPane changesContentPanel = new HTMLPane();
				changesContentPanel.setWidth100();
				changesContentPanel.setHeight("50%");
				changesContentPanel.setShowEdges(true);
				changesContentPanel.setContents(changesContent);
				changesContentPanel.setContentsType(ContentsType.FRAGMENT);

				Label separator = new Label(" ");
				separator.setHeight(10);

				panel.setMembers(notesLabel, notesContentPanel, separator, changesLabel, changesContentPanel);

				notesPanel.addMember(panel);
			}
		});
	}

	private void showContextMenu(ListGrid list) {
		ListGridRecord rec = list.getSelectedRecord();
		final GUIPatch patch = new GUIPatch();
		patch.setId(rec.getAttribute("id"));
		patch.setName(rec.getAttribute("name"));
		patch.setFile(rec.getAttribute("file"));
		patch.setDescription(rec.getAttribute(DESCRIPTION));
		patch.setSize(rec.getAttributeAsLong("size"));
		patch.setDate(rec.getAttributeAsDate("date"));
		patch.setInstalled(rec.getAttributeAsBoolean(INSTALLED));
		patch.setLocal(rec.getAttributeAsBoolean(LOCAL));

		Menu contextMenu = new Menu();

		MenuItem install = new MenuItem();
		install.setTitle(I18N.message("install"));
		install.addClickHandler(event -> switchDetailView(patch));
		install.setEnabled(!patch.isInstalled());

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> onDelete(patch.getFile()));
		delete.setEnabled(patch.isLocal());

		contextMenu.setItems(install, delete);
		contextMenu.showContextMenu();
	}

	private void onConfirm(GUIPatch patch) {
		SC.ask(I18N.message("confirmpatch"), I18N.message("confirmpatchquestion"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				confirmPatch.setVisible(false);
				download.setVisible(false);
				UpdateService.Instance.get().confirmPatch(patch.getFile(), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String path) {
						Session.get().setUpdating(true);
						cancel.setVisible(false);
						switchLogView(patch);
						lastConfirmed = new Date();
					}
				});
			}
		});
	}

	private void getStatus(GUIPatch patch) {
		LD.updatingServer();

		RequestBuilder builder = new RequestBuilder(RequestBuilder.GET,
				Util.contextPath() + "updatestatus?fileName=" + patch.getFile());

		try {
			builder.sendRequest(null, new RequestCallback() {
				public void onError(Request request, Throwable exception) {
					// Nothing to do
				}

				public void onResponseReceived(Request request, Response response) {
					if (response != null && response.getStatusCode() < 400) {
						String[] tokens = response.getText().split("\\|");
						String statusLabel = tokens[0];
						String command = tokens[1];
						String logContent = tokens[2];

						log.setValue(logContent);

						Date now = new Date();
						long elapsedTime = now.getTime() - lastConfirmed.getTime();

						if ("processed".equals(statusLabel)) {
							LD.clearPrompt();
							ok.setDisabled(patch.isRestart());
							GuiLog.info(I18N.message("patchinstalled"));
							if (patch.isRestart())
								Util.waitForUpAndRunning(Session.get().getTenantName(), I18N.getLocale());
						} else if (!"running".equals(statusLabel) && elapsedTime > MAX_WAIT_TIME) {
							LD.clearPrompt();
							ApplicationRestarting.get(I18N.message("patchnotstarted", command)).show();
						} else {
							scheduleGetStatus(patch);
						}
					}
				}
			});
		} catch (RequestException e) {
			// Nothing to do
		}
	}

	private void scheduleGetStatus(GUIPatch patch) {
		new Timer() {
			public void run() {
				getStatus(patch);
			}
		}.schedule(500);
	}
}