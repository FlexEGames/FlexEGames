package me.hsgamer.flexegames.game.duel;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.api.property.GamePropertyAdapter;
import me.hsgamer.flexegames.api.property.GamePropertyKeyValue;

@UtilityClass
public class DuelProperties {
    public static final GamePropertyKeyValue<String> WORLD = new GamePropertyKeyValue<>("world", "default", new GamePropertyAdapter<>(String.class));
    public static final GamePropertyKeyValue<Boolean> LEGACY_PVP = new GamePropertyKeyValue<>("legacy-pvp", false, new GamePropertyAdapter<>(Boolean.class));
    public static final GamePropertyKeyValue<String> KIT = new GamePropertyKeyValue<>("kit", "default", new GamePropertyAdapter<>(String.class));
}
