package com.logicaldoc.gui.frontend.client.personal;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
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
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.form.validator.MatchesFieldValidator;
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
		layout.setHeight(40);

		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setMinWidth(450);

		
		
		TextItem password = ItemFactory.newPasswordItem("password", "password", null);
		password.setRequired(true);
		password.setWrapTitle(false);
		LengthRangeValidator minSizeValidator = new LengthRangeValidator();
		minSizeValidator.setMin(6);
		password.setValidators(minSizeValidator);

		PasswordItem passwordAgain = ItemFactory.newPasswordItem("passwordagain", "passwordagain", null);
		passwordAgain.setRequired(true);
		passwordAgain.setWrapTitle(false);
		MatchesFieldValidator matchValidator = new MatchesFieldValidator();
		matchValidator.setOtherField("passwordagain");
		matchValidator.setErrorMessage(I18N.message("passwordnotmatch"));
		password.setValidators(minSizeValidator, matchValidator);

		StaticTextItem details = ItemFactory.newStaticTextItem("details", "details", Session.get().getUser()
				.getCertDN()
				+ " " + I18N.message("validtill") + ": " + I18N.formatDate(Session.get().getUser().getCertExpire()));
		details.setColSpan(2);
		details.setWrap(true);

		StaticTextItem hint = ItemFactory.newStaticTextItem("hint", "hint", I18N.message("typepasscert"));
		hint.setWrap(false);
		hint.setColSpan(2);
		hint.setShowTitle(false);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);

		IButton createNew = new IButton(I18N.message("generatecert"));
		createNew.setAutoFit(true);
		createNew.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (vm.validate()) {
					ContactingServer.get().show();
					SignService.Instance.get().generateNewCertificate(vm.getValueAsString("password"),
							new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg) {
									ContactingServer.get().hide();
									SecurityService.Instance.get().getUser(Session.get().getUser().getId(),
											new AsyncCallback<GUIUser>() {

												@Override
												public void onFailure(Throwable caught) {
													Log.serverError(caught);
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
							ContactingServer.get().show();
							SignService.Instance.get().deleteCertificate(new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg) {
									ContactingServer.get().hide();
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
			password.setRequired(false);
			passwordAgain.setRequired(false);
			form.setItems(details);
			layout.addMember(form);
			buttons.setMembers(delete);
		} else {
			Label label = new Label(I18N.message("youdonthavecert"));
			label.setHeight(50);
			label.setWrap(false);
			layout.addMember(label);
			form.setItems(hint, password, passwordAgain);
			layout.addMember(form);
			buttons.setMembers(createNew);
		}
		layout.addMember(buttons);

		addItem(layout);
	}
}
