package com.logicaldoc.gui.frontend.client.menu;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.frontend.client.account.CertificateDialog;
import com.logicaldoc.gui.frontend.client.account.ChangePassword;
import com.logicaldoc.gui.frontend.client.account.LastLogins;
import com.logicaldoc.gui.frontend.client.account.Profile;
import com.logicaldoc.gui.frontend.client.account.SignatureDialog;
import com.logicaldoc.gui.frontend.client.account.TrustedDevices;
import com.logicaldoc.gui.frontend.client.personal.contacts.Contacts;
import com.logicaldoc.gui.frontend.client.security.twofactorsauth.TwoFactorsAuthenticationDialog;
import com.logicaldoc.gui.frontend.client.subscription.PersonalSubscriptions;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Collects various actions for the user's account
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class AccountMenu extends Menu {

	public AccountMenu() {
		setShowShadow(true);
		setShadowDepth(3);

		MenuItem profile = new MenuItem(I18N.message("profile"));
		profile.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				SecurityService.Instance.get().getUser(Session.get().getUser().getId(), new AsyncCallback<GUIUser>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIUser user) {
						Profile profile = new Profile(user);
						profile.show();
					}
				});
			}
		});

		MenuItem contacts = new MenuItem(I18N.message("contacts"));
		contacts.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Contacts.get().show();
			}
		});

		MenuItem removeCookies = new MenuItem(I18N.message("removecookies"));
		removeCookies.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Session.get().getUser().setDocsGrid(null);
				Session.get().getUser().setHitsGrid(null);

				Session.get().getUser().setDocsGrid(null);
				SecurityService.Instance.get().saveInterfaceSettings(Session.get().getUser(),
						new AsyncCallback<GUIUser>() {
							@Override
							public void onFailure(Throwable e) {
								GuiLog.serverError(e);
							}

							@Override
							public void onSuccess(GUIUser usr) {
								CookiesManager.removeAllCookies();
								GuiLog.info(I18N.message("cookiesremoved"), null);
								Session.get().logout();
							}
						});
			}
		});

		MenuItem subscriptions = new MenuItem(I18N.message("subscriptions"));
		subscriptions.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				PersonalSubscriptions s = new PersonalSubscriptions();
				s.show();
			}
		});

		List<MenuItem> items = new ArrayList<MenuItem>();

		if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.ACCOUNT)) {
			items.add(profile);
			items.add(getSecurityMenuItem());

			if (Feature.enabled(Feature.STAMP) && com.logicaldoc.gui.common.client.Menu
					.enabled(com.logicaldoc.gui.common.client.Menu.ACCOUNT_SIGNATURE))
				items.add(getSignatureMenu());

			if (Feature.enabled(Feature.DIGITAL_SIGNATURE)
					&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.SIGNATURE))
				items.add(getDigitalSignatureMenu());

			if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CONTACTS))
				items.add(contacts);

			if (Feature.enabled(Feature.AUDIT) && com.logicaldoc.gui.common.client.Menu
					.enabled(com.logicaldoc.gui.common.client.Menu.SUBSCRIPTIONS))
				items.add(subscriptions);

			items.add(removeCookies);
		}

		MenuItem logout = new MenuItem(I18N.message("logout"));
		logout.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Session.get().logout();
			}
		});
		items.add(logout);

		setItems(items.toArray(new MenuItem[0]));
	}

	private MenuItem getDigitalSignatureMenu() {
		MenuItem certificate = new MenuItem(I18N.message("certificate"));
		certificate.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				CertificateDialog cert = new CertificateDialog();
				cert.show();
			}
		});

		return certificate;
	}

	private MenuItem getSignatureMenu() {
		MenuItem signature = new MenuItem(I18N.message("signature"));
		signature.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				SignatureDialog sig = new SignatureDialog();
				sig.show();
			}
		});

		return signature;
	}

	private MenuItem getSecurityMenuItem() {
		MenuItem changePswd = new MenuItem(I18N.message("changepassword"));
		changePswd.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				ChangePassword cp = new ChangePassword();
				cp.show();
			}
		});
		changePswd.setEnabled(!Session.get().isDemo() && Session.get().getUser().getSource() == 0);

		MenuItem lastLogins = new MenuItem(I18N.message("lastlogins"));
		lastLogins.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				LastLogins ll = new LastLogins();
				ll.show();
			}
		});

		MenuItem twofactorsauth = new MenuItem(I18N.message("twofactorsauth"));
		twofactorsauth.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				TwoFactorsAuthenticationDialog dialog = new TwoFactorsAuthenticationDialog(Session.get().getUser(),
						false);
				dialog.show();
			}
		});
		twofactorsauth.setEnabled(!Session.get().isDemo() && Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION)
				&& Session.get().getTenantConfigAsBoolean("2fa.enabled"));

		MenuItem trustedDevices = new MenuItem(I18N.message("trusteddevices"));
		trustedDevices.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				TrustedDevices td = new TrustedDevices();
				td.show();
			}
		});
		trustedDevices.setEnabled(!Session.get().isDemo() && Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION)
				&& Session.get().getTenantConfigAsBoolean("2fa.enabled"));

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		if (Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION)
				&& Session.get().getTenantConfigAsBoolean("2fa.enabled"))
			menu.setItems(changePswd, lastLogins, twofactorsauth, trustedDevices);
		else
			menu.setItems(changePswd, lastLogins);

		MenuItem securityItem = new MenuItem(I18N.message("security"));
		securityItem.setSubmenu(menu);

		return securityItem;
	}
}
