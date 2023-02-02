package com.logicaldoc.gui.frontend.client.account;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the form used to display and generate the user's certificate
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class CertificateDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private VLayout layout = null;

	public CertificateDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("certificate"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		refresh();
	}

	public void refresh() {
		boolean crtAlreadyGenerated = Session.get().getUser().getCertDN() != null;
		vm.clearValues();
		vm.clearErrors(false);

		if (layout != null) {
			layout.destroy();
		}

		layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setMinWidth(450);

		StaticTextItem details = ItemFactory.newStaticTextItem("details", Session.get().getUser().getCertDN()
				+ " " + I18N.message("validtill") + ": " + I18N.formatDate(Session.get().getUser().getCertExpire()));
		details.setColSpan(2);
		details.setWrap(true);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);

		IButton createNew = new IButton(I18N.message("generatecert"));
		createNew.setAutoFit(true);
		createNew.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (vm.validate()) {
					LD.contactingServer();
					SignService.Instance.get().generateNewCertificate(new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							LD.clearPrompt();
							SecurityService.Instance.get().getUser(Session.get().getUser().getId(),
									new AsyncCallback<GUIUser>() {

										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(GUIUser user) {
											Session.get().setUser(user);
											refresh();
										}
									});
						}
					});
				}
			}
		});

		IButton delete = new IButton(I18N.message("deletecert"));
		delete.setAutoFit(true);
		delete.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				SC.ask(I18N.message("deletecertwarn"), new BooleanCallback() {

					@Override
					public void execute(Boolean value) {
						if (value) {
							LD.contactingServer();
							SignService.Instance.get().deleteCertificate(new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									LD.clearPrompt();
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg) {
									LD.clearPrompt();
									Session.get().getUser().setCertDN(null);
									Session.get().getUser().setCertExpire(null);
									refresh();
								}
							});
						}
					}
				});

			}
		});

		if (crtAlreadyGenerated) {
			form.setItems(details);
			layout.addMember(form);
			buttons.setMembers(delete);
		} else {
			Label label = new Label(I18N.message("youdonthavecert"));
			label.setHeight(50);
			label.setWrap(false);
			layout.addMember(label);
			buttons.setMembers(createNew);
		}
		layout.addMember(buttons);

		addItem(layout);
	}
}