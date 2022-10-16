package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

public interface ComponentGameState extends GameState {
    default Component getDisplayNameAsComponent() {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(getDisplayName());
    }
}
