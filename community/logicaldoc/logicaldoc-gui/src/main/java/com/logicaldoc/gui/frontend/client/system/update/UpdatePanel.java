package com.logicaldoc.gui.frontend.client.system.update;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ApplicationRestarting;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.UpdateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.types.Visibility;
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
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Updates check panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class UpdatePanel extends VLayout {

	// Shows the install notes panel
	VLayout notesPanel = new VLayout();

	private IButton download = new IButton(I18N.message("download"));

	public UpdatePanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		if (!Feature.enabled(Feature.UPDATES)) {
			setMembers(new FeatureDisabled(Feature.UPDATES));
			return;
		}

		ContactingServer.get().show();
		UpdateService.Instance.get().checkUpdate(Session.get().getInfo().getUserNo(),
				Session.get().getInfo().getRelease(), new AsyncCallback<GUIParameter[]>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						GuiLog.serverError(caught);
						onUpdateUnavailable();
					}

					@Override
					public void onSuccess(GUIParameter[] parameters) {
						ContactingServer.get().hide();

						if (parameters == null) {
							onUpdateUnavailable();
						} else if (parameters.length == 1 && parameters[0].getName().equals("error")) {
							onUpdateTemporarilyUnavailable(parameters[0].getValue());
						} else if (parameters != null) {
							DynamicForm form = new DynamicForm();
							form.setWidth(300);
							form.setTitleOrientation(TitleOrientation.LEFT);

							StaticTextItem name = ItemFactory.newStaticTextItem("name", "name",
									Util.getValue("name", parameters));
							name.setRequired(true);

							StaticTextItem date = ItemFactory.newStaticTextItem("date", "date",
									Util.getValue("date", parameters));
							date.setRequired(true);

							StaticTextItem size = ItemFactory.newStaticTextItem("size", "size",
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

							VLayout download = prepareDownloadProgress(parameters);

							VLayout infoPanel = new VLayout();
							infoPanel.setMembersMargin(10);
							infoPanel.setMembers(form, download);

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
		label.setIcon("[SKIN]/Dialog/stop.png");
		label.setShowEdges(true);
        label.setValign(VerticalAlignment.CENTER);  
        label.setAlign(Alignment.LEFT);
        label.setWrap(true);
		addMember(label);
	}

	private void onUpdateTemporarilyUnavailable(String reason) {
		Label label = new Label(I18N.message("updatepackagetemporarilynotavailable",
				I18N.message("updatepackagetemporarilynotavailable." + reason)));
		label.setPadding(10);
		label.setWrap(false);
		label.setIcon("[SKIN]/Dialog/warn.png");
		label.setShowEdges(true);
        label.setValign(VerticalAlignment.CENTER);  
        label.setAlign(Alignment.LEFT);
        label.setWrap(true);
		addMember(label);
	}

	private VLayout prepareDownloadProgress(final GUIParameter[] parameters) {

		final String fileName = Util.getValue("file", parameters);

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

		final IButton confirmUpdate = new IButton(I18N.message("confirmupdate"));
		confirmUpdate.setAutoFit(true);
		confirmUpdate.setVisible(false);
		confirmUpdate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				SC.ask(I18N.message("confirmupdate"), I18N.message("confirmupdatequestion"), new BooleanCallback() {

					@Override
					public void execute(Boolean choice) {
						if (choice.booleanValue()) {
							confirmUpdate.setVisible(false);
							download.setVisible(false);
							UpdateService.Instance.get().confirmUpdate(fileName, new AsyncCallback<String>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(String path) {
									ApplicationRestarting
											.get(I18N.message("updaterunning", path.replaceAll("\\\\", "/"))).show();

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
							});
						}
					}
				});
			}
		});

		download = new IButton(I18N.message("download"));
		download.setAutoFit(true);
		download.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				bar.setPercentDone(0);
				download.setDisabled(true);
				UpdateService.Instance.get().downloadUpdate(Session.get().getInfo().getUserNo(),
						Util.getValue("id", parameters), fileName, Long.parseLong(Util.getValue("size", parameters)),
						new AsyncCallback<Void>() {

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
													confirmUpdate.setVisible(true);
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
		buttonCanvas.setMembers(download, confirmUpdate);

		layout.addMember(buttonCanvas);

		return layout;
	}

	private void displayNotes(String fileName) {
		UpdateService.Instance.get().getUpdateNotes(fileName, new AsyncCallback<String[]>() {

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

				TextAreaItem updatenotes = ItemFactory.newTextAreaItem("updatenotes", "updatenotes", infos[1]);
				updatenotes.setWidth("100%");
				updatenotes.setHeight(220);

				form.setItems(updatenotes, changelog);

				notesPanel.addMember(form);
			}
		});
	}
}