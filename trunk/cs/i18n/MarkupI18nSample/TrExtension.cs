using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Markup;

namespace MarkupI18nSample
{
    [MarkupExtensionReturnTypeAttribute(typeof(string))]
    public class TrExtension : MarkupExtension
    {

        string _key;

        public TrExtension(string key)
        {
            _key = key;
        }

        const string NotFoundError = "#StringNotFound#";

        public override object ProvideValue(IServiceProvider serviceProvider)
        {
            if (string.IsNullOrEmpty(_key))
                return NotFoundError;

            return Properties.Resources.ResourceManager.GetString(_key) ?? NotFoundError;
        }

    }


}
