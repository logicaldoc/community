package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.TwoFactorsAuthenticationService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Panel for setting up the Google Authenticator second factor authenticator.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class GoogleAuthenticatorSetup extends TwoFactorsAuthenticationSetup {

	private GUIUser user;
	
	public GoogleAuthenticatorSetup(final GUIUser user) {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);

		this.user=user;
	}

	private void init(String account, String key, String qrUrl) {
		// Prepare the form with account informations
		StaticTextItem accountItem = ItemFactory.newStaticTextItem("account", account);
		accountItem.setWrap(false);
		accountItem.setWrapTitle(false);

		StaticTextItem keyItem = ItemFactory.newStaticTextItem("key", key);
		keyItem.setWrap(false);
		keyItem.setWrapTitle(false);

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setItems(accountItem, keyItem);

		Label formTitle = new Label("<b>" + I18N.message(Constants.TWOFA_GOOGLE_AUTHENTICATOR+".hint1") + "</b>");
		formTitle.setWrap(true);
		formTitle.setMinWidth(250);

		VLayout formLayout = new VLayout();
		formLayout.setMembersMargin(2);
		formLayout.setMembers(formTitle, form);
		formLayout.setAlign(VerticalAlignment.TOP);

		// Prepare the QR Code
		HTMLFlow qrCode = new HTMLFlow("<img width='200' height='200' src='" + qrUrl + "' style='float:body' align='body'/>");
		qrCode.setOverflow(Overflow.VISIBLE);

		Label qrCodeTitle = new Label("<b>" + I18N.message(Constants.TWOFA_GOOGLE_AUTHENTICATOR+".hint2") + "</b>");
		qrCodeTitle.setWrap(true);
		qrCodeTitle.setMinWidth(250);

		VLayout qrCodeLayout = new VLayout();
		qrCodeLayout.setWidth100();
		qrCodeLayout.setHeight100();
		qrCodeLayout.setMembers(qrCodeTitle, qrCode);

		VLayout separator = new VLayout();
		separator.setWidth(20);

		HLayout body = new HLayout();
		body.setMembersMargin(2);
		body.setMembers(formLayout, separator, qrCodeLayout);

		setMembers(body);
	}

	@Override
	protected void onDraw() {
		account = user.getUsername() + "@" + WindowUtils.getRequestInfo().getHostName();

		TwoFactorsAuthenticationService.Instance.get().generateGoogleAuthorizationCredentials(account,
				new AsyncCallback<String[]>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String[] arg) {
						user.setKey(arg[0]);
						key = arg[0];
						init(account, arg[0], arg[1]);
					}
				});
	}
}