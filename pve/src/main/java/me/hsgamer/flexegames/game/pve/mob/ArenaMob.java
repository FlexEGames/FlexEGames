package me.hsgamer.flexegames.game.pve.mob;

import me.hsgamer.flexegames.game.pve.feature.ConfigFeature;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.attribute.Attribute;
import net.minestom.server.entity.EntityCreature;
import net.minestom.server.entity.EntityType;
import net.minestom.server.event.entity.EntityDamageEvent;
import net.minestom.server.event.entity.EntityDeathEvent;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ArenaMob extends EntityCreature {
    private static final int BLOCK_LENGTH = 6;
    private static final List<String> CHARACTERS = List.of(
            "", "▏", "▎", "▍",
            "▌", "▋", "▊", "▉"
    );
    private static final String FULL_BLOCK_CHAR = "█";
    final Arena arena;

    public ArenaMob(@NotNull EntityType entityType, Arena arena) {
        super(entityType);
        this.arena = arena;
        var stage = getStage();
        int multi = arena.getFeature(ConfigFeature.class).config().isToughMob() ? 2 : 1;
        getAttribute(Attribute.MAX_HEALTH).setBaseValue((getMaxHealth() + stage * 2) * multi);
        getAttribute(Attribute.ATTACK_DAMAGE).setBaseValue((1 + stage / 4f) * multi);
        heal();
        setCustomName(generateHealthBar(getMaxHealth(), getHealth()));
        setCustomNameVisible(true);
        eventNode()
                .addListener(EntityDamageEvent.class, event -> setCustomName(generateHealthBar(getMaxHealth(), getHealth())))
                .addListener(EntityDeathEvent.class, event -> setCustomName(generateHealthBar(getMaxHealth(), 0)));
    }

    @Contract(pure = true)
    private static @NotNull Component generateHealthBar(float maxHealth, float minHealth) {
        // Converts the health percentage into a number from 0-{blockLength} -- only 0 if the mob's health is 0
        final double charHealth = (minHealth / maxHealth) * BLOCK_LENGTH;
        return Component.text()
                .append(Component.text("[", NamedTextColor.DARK_GRAY))
                .append(Component.text(
                        FULL_BLOCK_CHAR.repeat((int) Math.floor(charHealth)),
                        NamedTextColor.RED
                )).append(Component.text(CHARACTERS.get((int) Math.round(
                        (charHealth - Math.floor(charHealth)) // number from 0-1
                                * (CHARACTERS.size() - 1) // indexes start at 0
                )), NamedTextColor.YELLOW))
                .append(Component.text("]", NamedTextColor.DARK_GRAY))
                .build();
    }

    protected int getStage() {
        return arena.getFeature(StageFeature.class).getStage();
    }
}