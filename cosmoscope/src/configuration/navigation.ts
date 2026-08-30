import type { ConfigSection, ConfigurationSnapshot } from '@/rpc/schemas'

export interface ConfigNavigationCluster {
  key: string
  label: string
  sections: readonly ConfigSection[]
  repeatable?: ConfigurationSnapshot['configuration']['repeatableSections'][number]
}

export interface ConfigNavigationGroup {
  key: string
  label: string
  clusters: readonly ConfigNavigationCluster[]
}

export function configSectionTitle(section: ConfigSection): string {
  return section.label
}

export function groupConfigSections(configuration: ConfigurationSnapshot['configuration']): readonly ConfigNavigationGroup[] {
  const groups = new Map<string, { label: string, clusters: Map<string, ConfigNavigationCluster> }>()
  const add = (groupKey: string, groupLabel: string, clusterKey: string, clusterLabel: string, section?: ConfigSection, repeatable?: ConfigNavigationCluster['repeatable']): void => {
    const group = groups.get(groupKey) ?? { label: groupLabel, clusters: new Map() }
    const cluster = group.clusters.get(clusterKey) ?? { key: clusterKey, label: clusterLabel, sections: [] }
    group.clusters.set(clusterKey, {
      ...cluster,
      label: repeatable === undefined ? cluster.label : clusterLabel,
      sections: section === undefined ? cluster.sections : [...cluster.sections, section],
      repeatable: repeatable ?? cluster.repeatable,
    })
    groups.set(groupKey, group)
  }

  for (const section of configuration.sections) {
    const repeatable = configuration.repeatableSections.find((candidate) =>
      candidate.path.length + 1 === section.path.length && candidate.path.every((segment, index) => section.path[index] === segment))
    const groupKey = JSON.stringify(section.group.path)
    add(groupKey, section.group.label, repeatable === undefined ? 'main' : JSON.stringify(repeatable.path), repeatable?.label ?? '', section, repeatable)
  }
  for (const repeatable of configuration.repeatableSections) {
    add(JSON.stringify(repeatable.group.path), repeatable.group.label, JSON.stringify(repeatable.path), repeatable.label, undefined, repeatable)
  }

  return [...groups].map(([key, group]) => ({ key, label: group.label, clusters: [...group.clusters.values()] }))
}
