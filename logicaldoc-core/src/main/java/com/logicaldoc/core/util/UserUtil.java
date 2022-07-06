package com.logicaldoc.core.util;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.imaging.ImageUtil;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.IdenticonAvatar;
import com.timgroup.jgravatar.Gravatar;
import com.timgroup.jgravatar.GravatarDefaultImage;
import com.timgroup.jgravatar.GravatarRating;

/**
 * Various utilities about users.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class UserUtil {

	/**
	 * A transparent 1x1 PNG
	 */
	private static final String TRANSPARENT_IMAGE = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=";

	protected static Logger log = LoggerFactory.getLogger(UserUtil.class);

	/**
	 * This method retrieves the user folder. If not exists, it creates the
	 * folder. The folder is: <b>conf.userdir</b>/<b>id</b>
	 * 
	 * @param id The user identifier
	 * 
	 * @return the user's data folder
	 */
	public static File getUserHome(long id) {
		File root = getUsersDir();
		File userDir = new File(root, Long.toString(id));
		if (!userDir.exists()) {
			try {
				FileUtils.forceMkdir(userDir);
			} catch (IOException e) {
				return null;
			}
		}
		return userDir;
	}

	/**
	 * This method retrieves a user resource (file or folder). If the resource
	 * is a folder and not exists, it creates the folder. The folder will be:
	 * <b>conf.userdir</b>/<b>id</b>/<b>path</b>
	 * 
	 * @param id The user identifier
	 * @param path The resource path
	 * 
	 * @return the user's resource
	 */
	public static File getUserResource(long id, String path) {
		File root = getUserHome(id);
		File resource = new File(root.getPath() + "/" + path);

		if (!resource.exists() && !path.contains("."))
			try {
				FileUtils.forceMkdir(resource);
			} catch (IOException e) {
				return null;
			}

		return resource;
	}

	/**
	 * This method retrieves the users root folder. If not exists, it creates
	 * the folder. The folder is: <b>conf.userdir</b>
	 * 
	 * @return the root folder that contains the users data folders
	 */
	public static File getUsersDir() {
		File userpath = new File("");
		try {
			ContextProperties conf = Context.get().getProperties();
			userpath = new File(conf.getPropertyWithSubstitutions("conf.userdir"));
			try {
				FileUtils.forceMkdir(userpath);
			} catch (IOException e) {
				return null;
			}
		} catch (Throwable t) {

		}
		return userpath;
	}

	/**
	 * Updates the avatar of a user with a given .png image
	 * 
	 * @param user The user to elaborate
	 * @param avatarImageFile The file containing the avatar image
	 */
	public static void saveAvatar(User user, File avatarImageFile) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenantName = tenantDao.getTenantName(user.getTenantId());

		int size = Context.get().getProperties().getInt(tenantName + ".gui.avatar.size", 128);

		File tmpAvatarImage = null;
		try {
			tmpAvatarImage = File.createTempFile("avatar", ".png");
			BufferedImage avatar = ImageIO.read(avatarImageFile);
			avatar = ImageUtil.cropCenterSquare(avatar, size);
			ImageIO.write(avatar, "png", tmpAvatarImage);
			user.setAvatar(ImageUtil.encodeImage(tmpAvatarImage));
			userDao.store(user);
		} catch (Throwable t) {
			log.warn("Error generating default the avatar for user {}", user, t);
		} finally {
			FileUtil.strongDelete(tmpAvatarImage);
		}
	}

	/**
	 * Updates the avatar of a user with the default avatar that is the Gravatar
	 * or the auto-generated image(in case Gravatar is not available)
	 * 
	 * @param user The user to elaborate
	 */
	public static void generateDefaultAvatar(User user) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenantName = tenantDao.getTenantName(user.getTenantId());

		int size = Context.get().getProperties().getInt(tenantName + ".gui.avatar.size", 128);

		File tmpAvatarImage = null;
		try {
			tmpAvatarImage = File.createTempFile("avatar", ".png");
			BufferedImage avatar = UserUtil.generateDefaultAvatarImage(user, size);
			ImageIO.write(avatar, "png", tmpAvatarImage);
			user.setAvatar(ImageUtil.encodeImage(tmpAvatarImage));

			if (user.getType() != User.TYPE_SYSTEM)
				userDao.store(user);
			else
				userDao.jdbcUpdate("update ld_user set ld_avatar = ? where ld_username = ?", user.getAvatar(),
						user.getUsername());
		} catch (Throwable t) {
			if (user.getType() == User.TYPE_DEFAULT)
				log.warn("Error generating default the avatar for user {}", user, t);
			else
				log.debug("Error generating default the avatar for user {}", user, t);
		} finally {
			FileUtil.strongDelete(tmpAvatarImage);
		}
	}

	private static BufferedImage generateDefaultAvatarImage(User user, int size) throws IOException {
		Gravatar gravatar = new Gravatar();
		gravatar.setRating(GravatarRating.GENERAL_AUDIENCES);
		gravatar.setDefaultImage(GravatarDefaultImage.GRAVATAR_ICON);
		gravatar.setSize(size);

		/*
		 * Check Gravatar with main email
		 */
		BufferedImage avatarImage = null;
		byte[] bytes;
		try {
			bytes = gravatar.download(user.getEmail());
			if (bytes != null && bytes.length > 0)
				avatarImage = ImageIO.read(new ByteArrayInputStream(bytes));
		} catch (Throwable t) {
			log.warn("Cannot download gravatar for email {}", user.getEmail(), t);
		}

		/*
		 * Check Gravatar with secondary email
		 */
		if (avatarImage == null && StringUtils.isNotEmpty(user.getEmail2())) {
			try {
				bytes = gravatar.download(user.getEmail2());
				if (bytes != null && bytes.length > 0)
					avatarImage = ImageIO.read(new ByteArrayInputStream(bytes));
			} catch (Throwable t) {
				log.warn("Cannot download gravatar for email {}", user.getEmail2(), t);
			}
		}

		/*
		 * Now generate one from scratch
		 */
		if (avatarImage == null) {
			try {
				Avatar avatar = IdenticonAvatar.newAvatarBuilder().size(size, size).build();
				avatarImage = avatar.create(user.getId() > 0 ? user.getId() : -user.getId());
			} catch (Throwable t) {
				log.warn("Cannot generate avatar for user {}", user, t);
			}
		}

		// If the image is bigger, then get the central square
		if (avatarImage != null && (avatarImage.getWidth() > size || avatarImage.getHeight() > size)) {
			avatarImage = ImageUtil.cropCenterSquare(avatarImage, size);
		}

		return avatarImage;
	}

	public static String getAvatarImage(String userIdOrName) {
		String content = TRANSPARENT_IMAGE;

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = null;
		if (userIdOrName != null)
			try {
				user = userDao.findById(Long.parseLong(userIdOrName));
			} catch (Throwable t) {
				// perhaps the id is the username
				user = userDao.findByUsername(userIdOrName);
			}

		if (user != null) {
			if (StringUtils.isEmpty(user.getAvatar()))
				UserUtil.generateDefaultAvatar(user);
			content = user.getAvatar();
		}

		return content;
	}
}