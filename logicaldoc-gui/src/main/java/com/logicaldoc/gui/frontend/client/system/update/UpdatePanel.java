package com.logicaldoc.gui.frontend.client.system.update;

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
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ApplicationRestarting;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.UpdateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
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
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Updates check panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class UpdatePanel extends VLayout {

	private static final long MAX_WAIT_TIME = 2L * 60L * 1000L; // 2 minutes

	// Shows the install notes panel
	VLayout notesPanel;

	private IButton download;

	private IButton upload;

	private IButton confirmUpdate;

	private IButton delete;

	private TextAreaItem log;

	private ButtonItem ok;

	private Date lastConfirmed = null;

	private String updateFileName;

	public UpdatePanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		refresh();
	}

	void refresh() {
		Util.removeChildren(this);

		notesPanel = new VLayout();

		download = new IButton(I18N.message("download"));

		upload = new IButton(I18N.message("uploadupdatepackage"));
		upload.setAutoFit(true);
		upload.addClickHandler(event -> new UpdateUploader(this).show());

		confirmUpdate = new IButton(I18N.message("confirmupdate"));

		delete = new IButton(I18N.message("ddelete"));

		if (!Feature.enabled(Feature.UPDATES)) {
			setMembers(new FeatureDisabled(Feature.UPDATES));
			return;
		}

		LD.contactingServer();

		UpdateService.Instance.get().checkUpdate(new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
				onUpdateUnavailable();
			}

			@Override
			public void onSuccess(List<GUIParameter> parameters) {
				LD.clearPrompt();

				if (parameters.isEmpty()) {
					onUpdateUnavailable();
				} else if (parameters.size() == 1 && parameters.get(0).getName().equals("error")) {
					onUpdateTemporarilyUnavailable(parameters.get(0).getValue());
				} else {
					updateFileName = Util.getValue("file", parameters);

					DynamicForm form = new DynamicForm();
					form.setWidth(300);
					form.setTitleOrientation(TitleOrientation.LEFT);

					StaticTextItem name = ItemFactory.newStaticTextItem("name", Util.getValue("name", parameters));
					name.setRequired(true);

					StaticTextItem date = ItemFactory.newStaticTextItem("date", Util.getValue("date", parameters));
					date.setRequired(true);

					StaticTextItem size = ItemFactory.newStaticTextItem("size",
							Util.formatSize(Long.parseLong(Util.getValue("size", parameters))));
					size.setRequired(true);

					StaticTextItem target = ItemFactory.newStaticTextItem("target", "updatesto",
							Util.getValue("target", parameters));
					target.setRequired(true);

					form.setItems(name, date, size, target);

					Label message = new Label();
					message.setContents(I18N.message("updatepackagefound"));
					message.setWrap(false);
					message.setAlign(Alignment.LEFT);
					message.setStyleName("updateavailable");
					message.setLayoutAlign(Alignment.LEFT);
					message.setLayoutAlign(VerticalAlignment.TOP);
					message.setHeight(20);

					VLayout downloadLayout = prepareActionsBar(parameters);

					VLayout infoPanel = new VLayout();
					infoPanel.setMembersMargin(10);
					infoPanel.setMembers(form, downloadLayout);

					HLayout body = new HLayout();
					body.setWidth100();
					body.setMembersMargin(50);
					notesPanel.setWidth100();
					body.setMembers(infoPanel, notesPanel);

					setMembers(message, body);
				}
			}
		});
	}

	private void onUpdateUnavailable() {
		Label label = new Label(I18N.message("updatepackagenotfound"));
		label.setPadding(10);
		label.setWrap(false);
		label.setIcon("[SKIN]/actions/accept.png");
		label.setShowEdges(true);
		label.setValign(VerticalAlignment.CENTER);
		label.setAlign(Alignment.LEFT);
		label.setWrap(true);
		addMember(label);
		addMember(upload);
	}

	private void onUpdateTemporarilyUnavailable(String reason) {
		Label label = new Label(I18N.message("updatepackagetemporarilynotavailable",
				I18N.message("updatepackagetemporarilynotavailable." + reason)));
		label.setPadding(10);
		label.setWrap(false);
		label.setIcon("[SKIN]/actions/help.png");
		label.setShowEdges(true);
		label.setValign(VerticalAlignment.CENTER);
		label.setAlign(Alignment.LEFT);
		label.setWrap(true);
		addMember(label);
		addMember(upload);
	}

	private VLayout prepareActionsBar(final List<GUIParameter> parameters) {
		final Label barLabel = new Label(I18N.message("downloadprogress"));
		barLabel.setHeight(16);
		barLabel.setWrap(false);

		final Progressbar bar = new Progressbar();
		bar.setHeight(24);
		bar.setVertical(false);
		bar.setLength(300);

		confirmUpdate.setAutoFit(true);
		confirmUpdate.addClickHandler(click -> onConfirm());

		delete.setAutoFit(true);
		delete.addClickHandler(click -> onDelete());

		download = new IButton(I18N.message("download"));
		download.setAutoFit(true);
		download.addClickHandler(event -> {
			bar.setPercentDone(0);
			download.setDisabled(true);
			UpdateService.Instance.get().downloadUpdate(Util.getValue("id", parameters), updateFileName,
					Long.parseLong(Util.getValue("size", parameters)), new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
							download.setDisabled(false);
						}

						@Override
						public void onSuccess(Void arg) {
							confirmUpdate.setVisible(false);

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

											if (status.get(1) == 100)
												onUpdatePackageLocallyAvailable(updateFileName);
											else
												schedule(50);
										}
									});
								}
							}.schedule(50);
						}
					});
		});

		boolean uploadFileAlreadyAvailableLocally = Util.getValue("updateFile", parameters) != null;
		download.setVisible(!uploadFileAlreadyAvailableLocally);
		confirmUpdate.setVisible(uploadFileAlreadyAvailableLocally);

		VLayout layout = new VLayout(4);
		layout.setWidth100();

		if (!uploadFileAlreadyAvailableLocally) {
			layout.addMember(barLabel);
			layout.addMember(bar);
		}

		HLayout buttonCanvas = new HLayout();
		buttonCanvas.setMembersMargin(6);
		buttonCanvas.setMembers(download, upload, confirmUpdate, delete);
		layout.addMember(buttonCanvas);

		if (uploadFileAlreadyAvailableLocally)
			onUpdatePackageLocallyAvailable(updateFileName);

		return layout;
	}

	private void onUpdatePackageLocallyAvailable(String fileName) {
		download.setVisible(false);
		confirmUpdate.setVisible(true);
		displayNotes(fileName);
	}

	private void displayNotes(String fileName) {
		UpdateService.Instance.get().getUpdateNotes(fileName, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<String> infos) {
				VLayout panel = new VLayout();

				Label notesLabel = new Label(I18N.message("updatenotes"));
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

	private void switchLogView() {
		Util.removeChildren(this);

		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("*");
		form.setTitleOrientation(TitleOrientation.TOP);

		log = ItemFactory.newTextAreaItem("log", "");
		log.setWidth("*");

		ok = new ButtonItem("ok", I18N.message("ok"));
		ok.addClickHandler(event -> Util.waitForUpAndRunning(Session.get().getTenantName(), I18N.getLocale()));
		ok.setDisabled(true);

		form.setItems(log, ok);

		addMember(form);

		getStatus();
	}

	private void getStatus() {
		LD.updatingServer();

		RequestBuilder builder = new RequestBuilder(RequestBuilder.GET,
				Util.contextPath() + "updatestatus?fileName=" + updateFileName);

		try {
			builder.sendRequest(null, new RequestCallback() {
				public void onError(Request request, Throwable exception) {
					scheduleGetStatus();
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
							ok.setDisabled(false);
							Util.uninstallCloseWindowAlert();
							GuiLog.info(I18N.message("updateinstalled"));
							Util.waitForUpAndRunning(Session.get().getTenantName(), I18N.getLocale());
						} else if (!"running".equals(statusLabel) && elapsedTime > MAX_WAIT_TIME && command != null
								&& !command.isEmpty()) {
							LD.clearPrompt();
							ApplicationRestarting.get(I18N.message("updatenotstarted", command)).show();
						}

						scheduleGetStatus();
					}
				}
			});
		} catch (RequestException e) {
			scheduleGetStatus();
		}
	}

	private void scheduleGetStatus() {
		new Timer() {
			public void run() {
				getStatus();
			}
		}.schedule(1000);
	}

	private void onConfirm() {
		SC.ask(I18N.message("confirmupdate"), I18N.message("confirmupdatequestion"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				confirmUpdate.setVisible(false);
				download.setVisible(false);
				UpdateService.Instance.get().confirmUpdate(updateFileName, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String path) {
						Session.get().setUpdating(true);
						switchLogView();
						lastConfirmed = new Date();
					}
				});
			}
		});
	}

	private void onDelete() {
		SC.ask(I18N.message("delete"), I18N.message("deleteupdatepackagequestion"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				UpdateService.Instance.get().deleteUpdate(updateFileName, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void v) {
						refresh();
					}
				});
			}
		});
	}
}