package me.hsgamer.flexegames.config.proxy;

import me.hsgamer.hscore.config.annotation.ConfigPath;

public interface VelocityConfig {
    @ConfigPath("secret")
    default String getSecret() {
        return "";
    }
}
